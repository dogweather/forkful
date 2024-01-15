---
title:                "Sending an http request"
html_title:           "C recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is an essential part of developing web applications and services. It allows communication between the client and server, enabling the exchange of data and resources. Understanding how to send HTTP requests is crucial for any programmer working in the web development field.

## How To

Sending an HTTP request in C programming language involves using the sockets library. Here is a simple example of sending a GET request to a website:

```C
#include <stdio.h> 
#include <sys/socket.h> 
#include <arpa/inet.h> 
#include <string.h> 

int main() 
{ 
    int socket_fd, valread; 
    struct sockaddr_in serv_addr; 

    char *request = "GET / HTTP/1.1\r\nHost: www.example.com\r\n\r\n"; 
    char buffer[1024] = {0}; 
    
    //create socket
    socket_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (socket_fd < 0) 
    { 
        printf("Socket creation error\n"); 
        return -1; 
    } 

    //set server address
    serv_addr.sin_family = AF_INET; 
    serv_addr.sin_port = htons(80);
    if(inet_pton(AF_INET, "93.184.216.34", &serv_addr.sin_addr)<=0)  
    { 
        printf("Invalid address/ Address not supported\n"); 
        return -1; 
    } 

    //connect to server
    if (connect(socket_fd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) 
    { 
        printf("Connection failed\n"); 
        return -1; 
    } 

    //send request
    send(socket_fd, request, strlen(request), 0); 
    printf("Request sent\n"); 
    
    //receive response
    valread = read(socket_fd, buffer, 1024); 
    printf("%s\n",buffer ); 
    return 0; 
} 
```

Sample output:
```
Request sent
HTTP/1.1 200 OK
Accept-Ranges: bytes
Cache-Control: max-age=604800
Content-Type: text/html
Date: Tue, 30 Mar 2021 00:00:00 GMT
Etag: "3147526947"
Expires: Tue, 06 Apr 2021 00:00:00 GMT
Last-Modified: Thu, 17 Oct 2019 07:18:26 GMT
Server: ECS (dcb/7F81)
Vary: Accept-Encoding
X-Cache: HIT
Content-Length: 1256

<!doctype html>
<html>
<head>
... (HTML code of the website)
```

## Deep Dive

There are various methods of sending HTTP requests in C, such as using the cURL library or directly implementing the HTTP protocol. The example above uses the sockets library, which provides low-level access to network communication. It is essential to handle errors properly when sending HTTP requests and to ensure the proper formatting of the request headers.

## See Also

- [cURL Library](https://curl.haxx.se/libcurl/)
- [HTTP Protocol Documentation](https://www.w3.org/Protocols/HTTP/1.1/rfc2616bis/draft-lafon-rfc2616bis-03.html)
- [Sockets Programming in C](https://www.geeksforgeeks.org/socket-programming-cc/)