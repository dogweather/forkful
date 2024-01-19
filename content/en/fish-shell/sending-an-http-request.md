---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Interactive Studio: Programming with Fish Shell - Sending HTTP Requests

## What & Why?
Sending an HTTP request is your means of asking a web server to give you stuff (document, image, video, etc.). Knowing how to do it is a must for any programmer, as it allows you to interact with APIs, collect data from websites, and submit data to online databases.

## How to:
Here's a simple way to send a GET request using curl in Fish shell:

```Fish Shell
curl https://example.com
```
Resulting data will be spat out right into your terminal.

To send a POST request with some data:

```Fish Shell
curl -X POST -d "name=fish&color=blue" https://example.com
```
This sends a 'name' and 'color' to the example.com server. Look out for JSON or XML data in response.

## Deep Dive
The `curl` command dates back to 1996. It's a Swiss Army knife for transferring data with URLs - making it a perfect tool for us. 

Fish shell runs `curl` just like any other Unix-like terminal. You have alternatives like `wget` for downloading or `httpie` for prettified, colorized HTTP traffic. They differ in syntax and functionality, but the essence - sending an HTTP request from terminal - stays the same.

## See Also
Explore more with:
- Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- cURL: [https://curl.se/](https://curl.se/)
- HTTPie: [https://httpie.io/](https://httpie.io/)
- Beginners guide to HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)

Go on and flex those Fish fins in the vast sea of web data!