---
title:                "Sending an http request"
html_title:           "PowerShell recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way for a computer system to communicate with a server to retrieve information or perform actions. Programmers use this method to allow their code to interact with web-based services and retrieve data or perform specific tasks.

## How to:

To send an HTTP request using PowerShell, we can use the `Invoke-WebRequest` command. This command takes in the URL of the server we want to communicate with and any additional parameters that may be required. We can also specify the type of HTTP request we want to send, such as GET, POST, or PUT.

Here's an example of sending a GET request to the GitHub API to retrieve information about a specific repository:

```PowerShell
Invoke-WebRequest -Uri https://api.github.com/repos/microsoft/PowerShell -Method Get
```

The output of this command will be in the form of a JSON string, which we can then parse and use in our code.

## Deep Dive:

The concept of HTTP requests dates back to the early 90s when the World Wide Web was first being developed. It allows for a standard way of communication between client-server systems.

Apart from using PowerShell's `Invoke-WebRequest` command, there are other ways to send an HTTP request, such as using the `WebRequest` class in .NET or using cURL in a command-line interface.

Under the hood, the `Invoke-WebRequest` command uses the .NET `HttpWebRequest` and `HttpWebResponse` classes to make the HTTP request and receive the response from the server.

## See Also:

- [Microsoft Docs on `Invoke-WebRequest` command](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Wikipedia page on HTTP requests](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)