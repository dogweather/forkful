---
title:                "Downloading a web page"
html_title:           "C++ recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page is the act of retrieving the code and content of a webpage from the internet. Programmers do this in order to access and utilize the information found on the webpage, whether it be for data analysis or web scraping.

## How to:
Before we can download a web page, we must first include the necessary libraries and headers. In our case, we will be using the "WinInet.h" header from the WinInet library to make HTTP requests. Here's an example of a simple function that downloads the HTML content of a webpage:

```C++
#include <Windows.h> 
#include <WinInet.h> 

void downloadWebpage(const char* url) { 

// HINTERNET is a handle representing an internet session 
HINTERNET hInternet = InternetOpenA("Mozilla/5.0 (compatible; MSIE10.0; Windows NT 6.2)", INTERNET_OPEN_TYPE_DIRECT, NULL, NULL, 0); 

// HINTERNET is also used as a handle for an opened URL 
HINTERNET hUrl = InternetOpenUrlA(hInternet, url, NULL, 0, INTERNET_FLAG_PRAGMA_NOCACHE | INTERNET_FLAG_KEEP_CONNECTION, 0); 

char data[2048]; 

// Read the content of the webpage into the data array 
InternetReadFile(hUrl, data, 2048, 0); 

// Display the downloaded content 
std::cout << data << std::endl; 

// Close the handle and free the memory 
InternetCloseHandle(hUrl); 
InternetCloseHandle(hInternet); 
} 

int main() { 
downloadWebpage("https://example.com"); 
return 0; 
}
```

**Output:**
```
<!doctype html>
<html>
<head>
<title>Example Domain</title>

<meta charset="utf-8" />
<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<style type="text/css">
body {
background-color: #f0f0f2;
margin: 0;
padding: 0;
font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;

...
```

## Deep Dive:
In the past, downloading a webpage was done using the "Winsock.h" header and functions like "socket()" and "connect()". However, this was a tedious and error-prone process, which is why the "WinInet.h" header was introduced. It provides simpler and more efficient functions for making HTTP requests.

Apart from using WinInet, you can also use the libCurl library to download web pages. It offers more advanced features and supports multiple protocols. However, due to its complexity, it may not be suitable for simple web page downloads.

When downloading a webpage, there are various HTTP status codes that can be returned, indicating the success or failure of the request. These codes can be accessed using the "HttpQueryInfoA()" function.

## See Also:
- [Microsoft Docs: WinInet](https://docs.microsoft.com/en-us/windows/win32/wininet/wininet)
- [libCurl Official Website](https://curl.haxx.se/libcurl/)