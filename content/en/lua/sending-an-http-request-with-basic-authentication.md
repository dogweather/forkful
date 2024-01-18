---
title:                "Sending an http request with basic authentication"
html_title:           "Lua recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication is a way for a program to access a website or API that requires authentication. Programmers use it to send sensitive data, such as passwords or API keys, securely to the server.

## How to:
### Sending a GET request with basic authentication
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
-- Make a URL with the username and password encoded in the URL
local url = "http://username:password@website.com"
-- Send the request and get the response
local response = {}
http.request{
  url = url,
  sink = ltn12.sink.table(response)
}
-- Print the response body
print(response[1])
```
Output:
```
<html>
  <head>
    <title>Sample Basic Auth Page</title>
  </head>
  <body>
    <h1>Welcome, username!</h1>
  </body>
</html>
```

### Sending a POST request with basic authentication
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
-- Make a table with the data to be sent in the body
local data = {
  username = "username",
  password = "password",
  message = "Hello World!"
}
-- Encode the data in the URL format
local body, boundary = encodeMultipartFormData(data)
-- Make a URL with the username and password encoded in the URL
local url = "http://website.com"
-- Send the request and get the response
local response = {}
http.request{
  url = url,
  method = "POST",
  headers = {
    ["Authorization"] = "Basic " .. mime.b64("username:password"),
    ["Content-Type"] = "multipart/form-data; boundary=" .. boundary
  },
  source = ltn12.source.string(body),
  sink = ltn12.sink.table(response)
}
-- Print the response body
print(response[1])
```
Output:
```
{"status": 200, "message": "Hello World!", "username": "username"}
```

## Deep Dive:
Sending HTTP requests with basic authentication has been a common practice since the early days of the internet. It is a simple authentication method where the username and password are encoded in the URL in the format ```http://username:password@website```.

An alternative to basic authentication is Digest Authentication, which adds an additional level of security by encoding the password in a non-reversible way. However, basic authentication is still widely used due to its simplicity and compatibility with most servers.

To implement basic authentication in Lua, the header must include the base64-encoded string of the username and password in the format "username:password", preceded by the word "Basic". This can be done using the Lua mime library.

## See Also:
- Lua Socket Documentation: https://github.com/diegonehab/luasocket
- LTN12 Documentation: https://github.com/keplerproject/ltn12
- Basic Authentication on Wikipedia: https://en.wikipedia.org/wiki/Basic_access_authentication