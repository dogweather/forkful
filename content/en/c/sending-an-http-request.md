---
title:                "C recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

HTTP requests are a fundamental concept in web programming and understanding how to send them is essential for building modern web applications. Whether you are building a simple website or a complex web application, being able to send HTTP requests allows your code to communicate with other servers and fetch data or resources.

## How To

To send an HTTP request in C programming, we first need to include the `stdio.h` and `stdlib.h` header files in our code. These will allow us to use functions for input and output operations and also for dynamic memory allocation. 

Next, we need to declare a variable to store the response we receive from the server. We can use `char` data type for this purpose. Then, we need to create a socket or a connection between our code and the server using the `socket()` function. This function takes in the address domain, socket type, and protocol as parameters.

Once we have a socket, we can connect to the server using the `connect()` function. We need to pass in the socket, server address, and port number as parameters. Now, we are ready to send our HTTP request using the `send()` function. We need to pass in our socket, the HTTP request message (in ASCII format), and the size of the message as parameters. 

The final step is to receive the response from the server using the `recv()` function. We need to pass in our socket, a buffer to store the response, and the size of the buffer as parameters. The response will be stored in the buffer and we can print it out using the `printf()` function.

The code for sending an HTTP request in C would look like this:

```
#include<stdio.h>
#include<stdlib.h>

int main() {
    // Declare variables
    char response[1024];
    int socket;

    // Create a socket
    socket = socket(AF_INET, SOCK_STREAM, 0);

    // Connect to server
    connect(socket, "example.com", 80);

    // Send HTTP request
    char *http_request = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
    send(socket, http_request, strlen(http_request), 0);

    // Receive response
    recv(socket, response, sizeof(response), 0);

    // Print response
    printf("%s", response);

    // Close connection
    close(socket);

    return 0;
}
```

Running this code would send a GET request to the specified server and print out the response received. 

## Deep Dive

Sending an HTTP request involves the use of different protocol layers such as the application layer, transport layer, and network layer. The `send()` function uses the application layer protocol (TCP) to create a connection with the server, while the `connect()` function establishes a Transport Control Protocol (TCP) connection.

The HTTP request itself is a simple text-based message that follows a specific format and contains information such as the request method, path, and host. The server then processes this request and sends back a response code to indicate the success or failure of the request.

It is important to ensure that the formatting of the HTTP request message is correct, otherwise the server may not be able to process it. Additionally, proper error handling should be implemented in case the connection or request fails.

## See Also

To learn more about sending HTTP requests in C, check out the following links:

- [C Socket Programming](https://www.geeksforgeeks.org/socket-programming-cc/)
- [Understanding HTTP Requests](https://www.tutorialspoint.com/http/http_requests.htm)
- [Socket Programming in C using TCP](https://www.geeksforgeeks.org/socket-programming-cc/)