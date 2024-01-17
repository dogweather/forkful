---
title:                "Sending an http request"
html_title:           "C# recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request means making a request to a server using the Hypertext Transfer Protocol (HTTP). This allows programmers to communicate with web servers and retrieve data or resources from them. It is a crucial aspect of web development as it enables the exchange of information between clients and servers.

## How to:

To send an HTTP request in C#, we can use the HttpClient class from the System.Net.Http namespace. Here is a simple example of sending an HTTP GET request:

```C#
// create an instance of HttpClient
HttpClient client = new HttpClient();

// make a GET request to a URL
HttpResponseMessage response = client.GetAsync("www.example.com").Result;

// read the response content
string responseContent = response.Content.ReadAsStringAsync().Result;

Console.WriteLine(responseContent); // output the response
```

This will send a GET request to the specified URL and retrieve the response, which can then be accessed and used in our program.

Another option is to use the WebClient class, which provides a higher-level interface for sending HTTP requests. Here is an example:

```C#
// create an instance of WebClient
WebClient client = new WebClient();

// download the response from a URL
byte[] response = client.DownloadData("www.example.com");

// convert the response to a string
string responseContent = Encoding.Default.GetString(response);

Console.WriteLine(responseContent); // output the response
```

## Deep Dive:

HTTP was developed in 1991 by Tim Berners-Lee as a part of the World Wide Web project. It is a request-response protocol, meaning the client makes a request and the server responds with the requested data. There are different types of HTTP requests such as GET, POST, PUT, and DELETE, each serving a specific purpose.

Besides using the HttpClient and WebClient classes, there are other alternatives for sending HTTP requests in C#. These include the HttpWebRequest and the WebSocket classes.

When sending an HTTP request, there are different aspects to consider, such as headers, content type, and authentication. The HttpClient class provides methods to set these parameters in the request.

## See Also:

- [Microsoft Docs - Sending HTTP requests in C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netcore-3.1)
- [Mozilla Developer Network - HTTP request methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [W3Schools - HTTP Tutorial](https://www.w3schools.com/whatis/whatis_http.asp)