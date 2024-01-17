---
title:                "Downloading a web page"
html_title:           "C recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page is the process of retrieving the HTML and other content of a webpage from a web server. This allows programmers to access and manipulate the information presented on a webpage for various purposes such as data analysis or content scraping.

## How to:

To download a web page in C, we can use the ```libcurl``` library which provides easy-to-use functions for HTTP requests. Here's an example of how to download the contents of a webpage and save it to a file named "output.html":

```
#include <curl/curl.h>
#include <stdio.h>

int main(void)
{
  CURL *curl;
  FILE *fp;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    fp = fopen("output.html", "wb");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    fclose(fp);
  }
  return 0;
}
```

This will create a file named "output.html" in the same directory as your C file containing the HTML of the webpage. You can easily modify this code to save the HTML to a string or print it directly to the console.

## Deep Dive:

Webpage downloading has been an important aspect of web development since the early days of the internet. Before libraries like ```libcurl``` were available, developers had to rely on low-level socket programming to make HTTP requests. Nowadays, there are alternative libraries such as ```libwebsockets``` and ```libmicrohttpd``` which provide more advanced functionalities for web requests.

To implement a more comprehensive HTTP client in C, one could dive into the details of HTTP protocol and handle various response codes and headers. However, using a library like ```libcurl``` is generally more efficient and handles these details for us.

## See Also:

Learn more about ```libcurl``` and its functionalities: https://curl.haxx.se/libcurl/ 

Alternative libraries for HTTP requests in C: ```libwebsockets``` - https://libwebsockets.org/, ```libmicrohttpd``` - https://www.gnu.org/software/libmicrohttpd/