# igmng

The simple tool that is the instagram followers tracker

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
│               │  port3000    │                │              │                │
│               │  http GET    │                │              │                │
│               │  /followers  │                │              │                │
│               ◄──────────────┤                ◄──────────────┤                │
│  igrouter     │              │ igmng client   │              │   DB (mysql)   │
│  http server  │              │    cron server │              │                │
│               ├──────────────►                ├──────────────►                │
│               │   json       │                │              │                │
│               │              │                │              │                │
│               │              │                │              │                │
└───────────────┘              └────────────────┘              └────────────────┘
```
