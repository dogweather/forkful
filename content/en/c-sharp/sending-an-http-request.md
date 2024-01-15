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

## Why

If you're a developer or programmer, chances are you've heard of HTTP requests. These requests are essential for web communication and enable users to interact with websites, APIs, and other web services. Learning how to send an HTTP request using C# can expand your skills and allow you to create dynamic and interactive web applications.

## How To

First, we need to import the `System.Net` namespace in our code, which contains the necessary classes and methods to make HTTP requests.

```C#
using System.Net;
```

Next, let's create an instance of the `WebRequest` class and specify the URL we want to send the request to.

```C#
WebRequest request = WebRequest.Create("https://www.example.com");
```

Then, we can use the `GetResponse()` method to send the request and receive a response from the server.

```C#
WebResponse response = request.GetResponse();
```

We can also specify the type of HTTP request we want to use, such as `GET`, `POST`, `PUT`, `DELETE`, etc., by setting the `Method` property of the `WebRequest` object.

```C#
request.Method = "POST";
```

To send data along with the request, we can use the `GetRequestStream()` method to get a stream and write our data to it.

```C#
Stream dataStream = request.GetRequestStream();
byte[] data = Encoding.UTF8.GetBytes("{"username":"john_doe", "password":"password123"}");
dataStream.Write(data, 0, data.Length);
```

Finally, we can read the response from the server using the `GetResponseStream()` method and convert it into a readable format.

```C#
using (Stream responseStream = response.GetResponseStream())
{
    StreamReader reader = new StreamReader(responseStream);
    string responseString = reader.ReadToEnd();
    Console.WriteLine(responseString);
}
```

## Deep Dive

When sending an HTTP request, there are different statuses and error codes that can be returned by the server. For example, a status code of 200 means the request was successful, while a status code of 404 means the requested resource was not found.

In addition, we can also add headers to our request using the `Headers` property of the `WebRequest` object. This allows us to include additional information, such as authentication, in our request.

```C#
request.Headers.Add("Authorization", "Bearer token123");
```

It's also important to note that when sending sensitive information, such as usernames and passwords, we should use a secure and encrypted connection by using the `https` protocol.

## See Also

- [Microsoft Docs: Sending HTTP Requests in C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.webrequest?view=net-5.0)
- [Codeburst: How to Make HTTP Requests in C#](https://codeburst.io/how-to-make-http-requests-in-c-615e0bb91c2a)
- [Pluralsight: C# Fundamentals Course](https://www.pluralsight.com/courses/csharp-fundamentals-with-c-sharp-5-0)