---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way for a computer to communicate with a web server. It allows developers to retrieve data from a specific URL and receive a response back.

Programmers use HTTP requests to make web-based applications and websites interactive. It enables the exchange of data between the client (user's computer) and the server, making the user experience more dynamic and responsive.

## How to:

To send an HTTP request using Bash, we will use the `curl` command, which stands for "client URL". Here's an example of how it works:

```Bash
curl https://www.example.com
```

This will send a GET request to the specified URL and output the response from the server. To specify a different type of request, such as POST or PUT, we can use the `-X` flag followed by the desired method. For example:

```Bash
curl -X POST https://www.example.com
```

We can also add additional parameters to our requests using the `-d` flag, which stands for "data". For example, to send a POST request with some data, we can do this:

```Bash
curl -X POST -d "name=John&age=25" https://www.example.com
```

## Deep Dive

HTTP requests were first introduced in 1991 as a way to transfer data over the internet. Since then, there have been different versions of the protocol, and it has become the standard for communication between clients and servers.

While `curl` is the most commonly used tool for sending HTTP requests in Bash, there are other alternatives such as `wget` and `httpie`. These tools offer additional features and options, so it's worth exploring them to find the one that best suits your needs.

Behind the scenes, sending an HTTP request involves establishing a TCP connection between the client and server, sending the request in a specific format, and receiving a response back. This process is handled by the network stack on the operating system level.

## See Also

- [Curl Documentation](https://curl.se/docs/)
- [Wget Documentation](https://www.gnu.org/software/wget/)
- [Httpie Documentation](https://httpie.io/)