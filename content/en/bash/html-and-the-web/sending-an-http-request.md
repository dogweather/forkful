---
date: 2024-01-20 17:58:50.569489-07:00
description: "How to: Bash can use tools like `curl` or `wget` for HTTP requests.\
  \ Here\u2019s a quick example with `curl`."
lastmod: '2024-03-13T22:45:00.242057-06:00'
model: gpt-4-1106-preview
summary: Bash can use tools like `curl` or `wget` for HTTP requests.
title: Sending an HTTP request
weight: 44
---

## How to:
Bash can use tools like `curl` or `wget` for HTTP requests. Here’s a quick example with `curl`.

```Bash
# Get the content of a webpage
curl https://example.com

# Post data to a server
curl -d "param1=value1&param2=value2" -X POST https://example.com/post-endpoint

# Include headers in a GET request
curl -H "Content-Type: application/json" https://example.com
```

Sample `curl` response:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive
HTTP requests have been around since the early '90s and are the foundation of web communication. `curl` and `wget` are Unix command-line tools introduced in 1996 and 1996, respectively, for network requests.

`wget` is typically used for downloading files, while `curl` can handle a wide variety of protocols and offers more features, making it a go-to for sending HTTP requests from the command line.

Implementing an HTTP request using these tools involves crafting the proper request headers, method (GET, POST, PUT, DELETE, etc.), and sometimes data payloads. Doing this from Bash scripts enables automation of interaction with web-based services.

Alternative ways of sending HTTP requests in scripting include using scripting languages like Python with libraries such as `requests`, or using tools like `httpie` for a more human-friendly interface.

## See Also
- curl official site: https://curl.se/
- wget manual: https://www.gnu.org/software/wget/manual/wget.html
- HTTPie: https://httpie.io/
- The Bash Academy: https://www.bash.academy/
- W3C HTTP Specifications: https://www.w3.org/Protocols/
