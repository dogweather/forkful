---
title:                "C recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##Why
Sending HTTP requests with basic authentication is a common task in web development. It allows for secure access to protected resources on a server by requiring users to provide login credentials before accessing the information. In this blog post, we will explore how to send an HTTP request with basic authentication using the C programming language.

##How To
To send an HTTP request with basic authentication in C, we can use the Curl library. First, we need to include the necessary header files:

```
#include <stdio.h>
#include <curl/curl.h>
```

Next, we need to initialize a Curl handle and set the URL we want to send the request to:

```
CURL *curl;
curl_global_init(CURL_GLOBAL_ALL); // initialize the Curl library
curl = curl_easy_init(); // create a Curl handle
curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");
```

We also need to specify the username and password for the basic authentication in the request headers:

```
struct curl_slist *headers = NULL;
headers = curl_slist_append(headers, "Authorization: Basic <base64 encoded username:password>");
curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
```

Now, we can send the request using the `curl_easy_perform()` function and check the response code and body of the request:

```
CURLcode res = curl_easy_perform(curl);
long response_code;
curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code); // get response code
if (res == CURLE_OK && response_code == 200) { // check if request was successful
    char *body; // variable to store response body
    curl_easy_getinfo(curl, CURLINFO_BODY, &body);
    printf("Request successful!\nBody: %s\n", body); // print response body
}
else {
    printf("Unable to send request. Error code: %d\n", res); // print error code
}
```

The output of the above code should look like this:

```
Request successful!
Body: This is the response body.
```

##Deep Dive
Behind the scenes, basic authentication works by sending the username and password in the form of a base64-encoded string in the `Authorization` header of the HTTP request. The server then decodes the string and compares it to the credentials stored on the server. If they match, the server allows access to the protected resource.

It is important to note that basic authentication is not a secure method of authentication as the username and password are sent in plain text and can be easily intercepted by a third party. It is recommended to use more secure methods such as OAuth or SSL instead.

##See Also
- [Curl documentation](https://curl.haxx.se/libcurl/c/)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)