---
title:                "Sending an http request with basic authentication"
html_title:           "C++ recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a way for programmers to securely access information from a web server. It involves adding a username and password to the request header, allowing the server to verify the identity of the sender. This is important for protecting sensitive information and ensuring that only authorized users have access.

## How to:

To send an HTTP request with basic authentication in C++, you can use the CPPREST library or the libcurl library. First, include the necessary headers for the library you chose. Then, create a client object and set the credentials using the `set_credentials()` function. Finally, make the request with the `request()` function and handle the response accordingly.

```C++
//Using the CPPREST library:
#include <cpprest/http_client.h>
#include <cpprest/basic_utils.h>

//Create client object
web::http::client::http_client client("https://example.com");

//Set credentials
client.set_credentials(web::credentials(username, password));

//Make request
web::http::http_response response = client.request(web::http::methods::GET).get();

//Using the libcurl library:
#include <curl/curl.h>

//Create client object
CURL* curl = curl_easy_init();

//Set credentials
curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

//Make request
CURLcode res = curl_easy_perform(curl);

//Handle response
if(res == CURLE_OK) {
  long response_code;
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
  //Handle response code
}

//Clean up
curl_easy_cleanup(curl); 
```

## Deep Dive

Sending HTTP requests with basic authentication has been a commonly used method for accessing web resources since the early days of the internet. However, it is not the most secure option available and should be used carefully, especially when working with sensitive data.

There are other types of authentication, such as OAuth and API keys, which may be more suitable depending on the specific use case. These alternatives can offer additional levels of security and flexibility, but may also require more setup and configuration.

In terms of implementation, the HTTP header for basic authentication consists of the keyword "Basic" followed by a base64-encoded string of the username and password separated by a colon. This means that the credentials are not encrypted, making it important to only use this method over a secure connection (HTTPS).

## See Also

To learn more about HTTP requests and how to implement them in C++, check out the following resources:

- [CPPREST library documentation](https://github.com/microsoft/cpprestsdk/wiki)
- [libcurl documentation](https://curl.haxx.se/libcurl/)
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc7235#section-2.1) (RFC 7235)