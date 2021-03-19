# igmng

The simple tool that is the instagram followers tracker

```
┌───────────────┐
│               │
│               │
│   Instagram   │
│               │
│               │
└───────▲───────┘
        │
        │
        │ Login authentication
        │
┌───────▼───────┐          ┌────────────────┐           ┌────────────────┐
│               │ port3000 │                │           │                │
│               │ http GET │                │           │                │
│               │ /follow  │                │           │                │
│               ◄──────────┤                ◄───────────┤                │
│  igrouter     │          │ igmng client   │           │   DB (mysql)   │
│  http server  │          │    cron server │           │                │
│               ├──────────►                ├───────────►                │
│               │   json   │                │           │                │
│               │          │                │           │                │
│               │          │                │           │                │
└───────────────┘          └────────────────┘           └────────────────┘
```
