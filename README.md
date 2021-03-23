# igmng

The simple tool that is the [Instagram](https://www.instagram.com/) followers tracker

```
┌───────────────┐
│               │
│               │
│   Instagram   │
│     (IG)      │
│               │
└───────▲───────┘
        │
        │ Login authentication
        │ Get follower infomation with IG API
        │
┌───────▼───────┐              ┌────────────────┐              ┌────────────────┐
│               │  port 3000   │                │              │                │
│               │  http GET    │                │              │                │
│               │  /followers  │                │              │                │
│               ◄──────────────┤                ◄──────────────┤                │
│  igrouter     │              │ igmng client   │              │   DB (mysql)   │
│  http server  │              │    cron server │              │                │
│  (Python)     ├──────────────► (Haskell)      ├──────────────►                │
│               │     json     │                │              │                │
│               │              │                │              │                │
│               │              │                │              │                │
└───────────────┘              └────────────────┘              └────────────────┘
```

## Usage

```
$ git clone git@github.com:falgon/igmng.git
$ cd igmng
$ cat > ./containers/.env <<EOS
MYSQL_DATABASE=dbname
MYSQL_ROOT_PASSWORD=rootpassword
MYSQL_USER=username
MYSQL_PASSWORD=userpassword
EOS
$ mkdir -p ~/.igmng/
$ cat > ~/.igmng/credentials.toml <<EOS
[credentials]
oath_key = "oath_key" # when you use two-factor code, it will be used by authentication.
user_id = "user_id"
password = "password"
EOS
$ cd ./containers
$ docker-compose up -d && cd ../
$ stack exec igmng -- check
```
