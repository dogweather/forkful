---
title:                "C recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

If you have ever wondered how web browsers are able to display colorful and interactive web pages, then you have come to the right place. In this article, we will discuss the process of downloading a web page using the C programming language.

## How To

In order to download a web page using C, we need to use a combination of networking and file handling functions. First, we need to establish a connection with the server in which the web page is hosted. This can be done using the `socket()` function, which creates a socket that enables communication between the client (our code) and the server.

Next, we need to send an HTTP request to the server using the `send()` function. This request will contain the URL of the web page we want to download. The server will then send back a response, which we can receive using the `recv()` function.

Once we have received the response, we need to save it into a file. We can do this by opening a file using the `fopen()` function and writing the received data into it using the `fwrite()` function. Finally, we can close the socket and the file using the `close()` and `fclose()` functions respectively.

To better understand this process, let's take a look at a code snippet:

```C
#include <stdio.h>
#include <sys/socket.h>
#include <arpa/inet.h>

int main()
{
    //create socket
    int client_socket = socket(AF_INET, SOCK_STREAM, 0);
    
    //define server address
    struct sockaddr_in serv_addr;
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    serv_addr.sin_port = htons(80); //http port
    
    //connect to server
    connect(client_socket, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
    
    //send HTTP request
    char request[] = "GET /index.html HTTP/1.1\r\n\r\n";
    send(client_socket, request, sizeof(request), 0);
    
    //receive response
    char buffer[1024] = {0};
    recv(client_socket, buffer, sizeof(buffer), 0);
    
    //save response to file
    FILE *file = fopen("index.html", "wb");
    fwrite(buffer, sizeof(char), sizeof(buffer), file);
    fclose(file);

    //close socket
    close(client_socket);

    return 0;
}
```

## Deep Dive

Downloading a web page using C may seem like a simple process, but there are many underlying concepts and protocols involved. One of the main protocols used is the HTTP (Hypertext Transfer Protocol), which is responsible for the communication between the client and server.

The server responds with a status code along with the requested data. This code tells the client if the request was successful or if there was an error. This is why we need to check the status code before saving the data into a file or displaying it.

Furthermore, web pages often contain various types of data such as images, videos, and scripts. In order to download and display the web page properly, we need to handle these different types of data accordingly.

In addition, downloading web pages using C also has its limitations. For example, it may not support some modern web technologies such as AJAX, which relies heavily on JavaScript. This is because C is a low-level programming language and is not designed specifically for web development.

## See Also

To learn more about downloading web pages using C, check out these helpful resources:

- https://www.w3.org/Protocols/
- https://www.geeksforgeeks.org/socket-programming-in-c-c/
- https://www.tutorialspoint.com/http/http_status_codes.htm