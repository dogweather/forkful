---
title:                "Sending an HTTP request"
aliases: - /en/bash/sending-an-http-request.md
date:                  2024-01-20T17:58:50.569489-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way to communicate with web servers to retrieve data or submit forms. Programmers do it to interact with web services, APIs or to automate tasks involving web content.

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
