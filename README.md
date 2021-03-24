# igmng

The simple tool that is the [Instagram](https://www.instagram.com/) followers tracker

```
┌───────────────┐              ┌───────────────┐
│               │              │               │
│               │              │               │
│   Instagram   │              │  LINE Notify  │
│     (IG)      │              │               │
│               │              │               │
└───────▲───────┘              └───────▲───────┘
        │                              │
        │ Login authentication         │
        │ Get follower infomation      │
        │ with IG API                  │
┌───────▼───────┐              ┌───────▼────────┐              ┌────────────────┐
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

```bash
$ stack exec igmng -- --help
Usage: igmng [-n|--no-fetch] [--enable-line-notify] [--env-file-path <filepath>]
             [--credentials-file-path <filepath>] [--limit <delete log number>]
             [--year-ago yyyy] COMMAND
  instagram followers logger

Available options:
  -h,--help                Show this help text
  -n,--no-fetch            Does not fetch followers status
  --enable-line-notify     enable line notify
  --env-file-path <filepath>
                           The .env file path
  --credentials-file-path <filepath>
                           credentials.toml file
  --limit <delete log number>
                           the number of deleting
  --year-ago yyyy          the number of delete year

Available commands:
  check                    fetch and check latest diff
  fetch                    fetch followers status
  delete                   delete logs
```

simple run

```bash
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

[line]
oauth_key = "line notify api oauth key"
EOS
$ cd ./containers
$ docker-compose up -d && cd ../
$ stack exec igmng -- fetch
$ stack exec igmng -- check --enable-line-notify
```

When unfollow is detected,
it will be notified using [LINE Notify](https://notify-bot.line.me/).

![LINE_capture](https://user-images.githubusercontent.com/1241783/112335577-b4a20c00-8cff-11eb-947f-31b9ba4a35af.jpg)
