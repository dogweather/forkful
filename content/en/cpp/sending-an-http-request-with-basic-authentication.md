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

## Why
Sending an HTTP request with basic authentication is necessary for accessing resources that require user authentication, such as a website or API. This ensures a secure connection and allows for proper identification of the user.

## How To
To send an HTTP request with basic authentication in C++, we will be using the [CURL library](https://curl.haxx.se/libcurl/). This library allows us to make requests to URLs and supports different authentication methods including basic authentication.

First, we need to include the CURL library in our code:
```C++
#include <curl/curl.h>
```

Next, we will set up our CURL object and specify the URL we want to make a request to:
```C++
// Initialize CURL object
CURL *curl;
curl = curl_easy_init();

// Specify URL
char url[] = "https://www.example.com";
```

Now, we need to specify the credentials for basic authentication. This includes the username and password encoded in Base64 format. We can do this by creating a string with the following format: username:password, and then using the [Base64 encoding function](https://www.base64encode.org/) to encode the string.
```C++
// Set credentials
char credentials[] = "username:password"; // replace with actual credentials
char encoded_credentials[] = "encoded_credentials_here"; // replace with encoded credentials
```

Next, we need to set the appropriate HTTP headers for basic authentication:
```C++
// Set HTTP headers
struct curl_slist *headers = NULL;
headers = curl_slist_append(headers, "Content-Type: application/json");
headers = curl_slist_append(headers, "Authorization: Basic " + std::string(encoded_credentials));
```

Now, we can make the actual HTTP request using the `curl_easy_setopt()` function:
```C++
// Make HTTP request
curl_easy_setopt(curl, CURLOPT_URL, url);
curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");
curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
```

Finally, we can execute the request and print the response:
```C++
// Execute request
CURLcode res = curl_easy_perform(curl);

// Check for errors
if(res != CURLE_OK) {
    fprintf(stderr, "Curl error: %s\n", curl_easy_strerror(res));
}

// Print response
printf("%s\n", response.c_str());
```

That's it! You have successfully sent an HTTP request with basic authentication in C++.

## Deep Dive
In the above example, we used the `CURLOPT_CUSTOMREQUEST` option with the value of "GET". This tells CURL to make a GET request to the specified URL. However, depending on the API or website you are accessing, you may need to use a different HTTP method such as POST or PUT. You can change the value of `CURLOPT_CUSTOMREQUEST` accordingly.

Additionally, you may need to include other HTTP headers in your request, depending on the requirements of the API or website. You can add these headers using the `curl_slist_append()` function, as shown in the example.

Also, remember that for basic authentication, your credentials need to be encoded in Base64 format. You can use any online tool or function to encode your credentials before setting them in the `Authorization` header.

## See Also
- [CURL library documentation](https://curl.haxx.se/docs/)
- [Base64 encoding function](https://www.base64encode.org/)