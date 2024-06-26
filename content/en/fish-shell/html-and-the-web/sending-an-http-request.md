---
date: 2024-01-20 17:59:35.692464-07:00
description: 'How to: Fish doesn''t have built-in commands for sending HTTP requests,
  but you can use `curl` right from the shell.'
lastmod: '2024-03-13T22:45:00.471454-06:00'
model: gpt-4-1106-preview
summary: Fish doesn't have built-in commands for sending HTTP requests, but you can
  use `curl` right from the shell.
title: Sending an HTTP request
weight: 44
---

## How to:
Fish doesn't have built-in commands for sending HTTP requests, but you can use `curl` right from the shell:

```Fish
curl http://api.example.com/data
```

For a POST request with JSON data:

```Fish
curl -X POST -H "Content-Type: application/json" -d '{"key":"value"}' http://api.example.com/data
```

To store the response:

```Fish
set response (curl -X GET http://api.example.com/data)
```

And here's what you might see after a GET request:

```Fish
{
  "response": "Some data from the server"
}
```

## Deep Dive
Historically, UNIX and Linux shells are handy for networking tasks. In the early days, tools like `telnet` were common for such purposes. Today, utility programs like `curl` and `wget` are the go-to. `curl` is a versatile tool that supports multiple protocols, and it's often used because of its simplicity and flexibility.

Python or Node.js can be used when you need more complex request handling. But for quick tasks or simple scripts, `curl` in Fish is efficient and effective.

Implementing an HTTP request through Fish usually means relying on third-party tools. Fish itself is designed to be a smart and user-friendly command-line shell, not a do-all tool. When you combine it with the power of utilities like `curl`, you get the best of both worlds: Fish's usability and `curl`'s capability.

## See Also
- Learn more about `curl`: https://curl.se/docs/manual.html
- Fish Shell documentation: https://fishshell.com/docs/current/index.html
- HTTP basics overview: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- Explore APIs with `httpie`, an alternative to `curl`: https://httpie.io/
