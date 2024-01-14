---
title:                "C++ recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request with basic authentication can be necessary when accessing a secure server or API that requires authentication. It allows for secure communication between the client and server, ensuring that only authorized users can access sensitive information.

## How To

To send an HTTP request with basic authentication in C++, we will use the cURL library. First, we need to initialize the cURL session and set the URL we want to send the request to:

```C++
CURL* curl;
curl = curl_easy_init();
curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api");
```

Next, we need to set the HTTP request type as `GET` or `POST` and specify that we want to use basic authentication:

```C++
curl_easy_setopt(curl, CURLOPT_HTTPGET, 1); // set request type as GET
curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC); // use basic authentication
```

Then, we need to set the username and password for the authentication:

```C++
curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
```

Finally, we can execute the request and retrieve the response from the server:

```C++
CURLcode res;
res = curl_easy_perform(curl); // execute the request

// check for errors
if (res != CURLE_OK) {
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
    curl_easy_strerror(res));
}

// retrieve response
long response_code;
double total_time;
curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code); // get response code
curl_easy_getinfo(curl, CURLINFO_TOTAL_TIME, &total_time); // get total time of request
```

Sample output for a successful request would be:

```C++
Response code: 200
Total time taken: 0.1234 seconds
```

## Deep Dive

HTTP basic authentication works by sending the username and password in plain text in the `Authorization` header of the request. This means that the authentication is not secure and can be easily intercepted.

To overcome this security issue, it is recommended to use HTTPS instead of HTTP when sending an HTTP request with basic authentication. This ensures that the communication between the client and server is encrypted and cannot be intercepted.

Additionally, in some cases, the server may require a different format for the `Authorization` header. In these cases, you will need to use the `curl_easy_setopt()` function to set the `CURLOPT_HTTPHEADER` option and provide the custom `Authorization` header.

## See Also

- cURL documentation: https://curl.se/docs/
- HTTP basic authentication: https://www.httpwatch.com/httpgallery/authentication/#basic