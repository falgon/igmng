#!/usr/bin/env python3
from auth import login
import io
import json
import sys
from instaloader import ConnectionException, Profile
from flask import Flask, jsonify, abort, make_response
from flask_log_request_id import RequestID, RequestIDLogFilter
from gevent.pywsgi import WSGIServer
import logging
import joblib
import os

app = Flask(__name__)
RequestID(app)
profile_jb = "profile.jb"


def config_logger():
    handler = logging.StreamHandler()
    handler.setFormatter(
        logging.Formatter(
            "%(asctime)s - %(name)s - level=%(levelname)s - request_id=%(request_id)s - %(message)s"
        )
    )
    handler.addFilter(RequestIDLogFilter())
    logging.getLogger().addHandler(handler)


config_logger()
logger = app.logger


def enter_ig() -> Profile:
    logger.setLevel(logging.INFO)
    logger.info("Start login")
    profile = login()
    if profile.is_left():
        logger.SetLevel(logging.ERROR)
        logger.error(profile.from_left)
        exit(1)
    logger.info("Done login")
    return profile.from_right


def get_profile() -> Profile:
    if os.path.exists(profile_jb):
        return joblib.load(profile_jb)
    else:
        profile = enter_ig()
        joblib.dump(profile, profile_jb, compress=3)
        return profile


profile = get_profile()


@app.route("/followers", methods=["GET"])
def get_followers():
    logger.setLevel(logging.INFO)
    logger.info("Start fetching data")
    result = {
        "followees_num": profile.followees,
        "followers_num": profile.followers,
        "followers": [
            {
                "user_id": x.userid,
                "name": x.username,
            }
            for x in profile.get_followers()
        ],
    }
    logger.info("Done fetching data")
    return make_response(json.dumps(result, ensure_ascii=False))


@app.errorhandler(404)
def not_fonud(error):
    return make_response(jsonify({"error": "Not found"}), 404)


if __name__ == "__main__":
    http_server = WSGIServer(("", 3000), app)
    http_server.serve_forever()
