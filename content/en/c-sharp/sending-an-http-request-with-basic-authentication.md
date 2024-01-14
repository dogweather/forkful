---
title:                "C# recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why: The purpose of sending an HTTP request with basic authentication

Sending an HTTP request with basic authentication is a crucial part of web development, as it ensures secure communication between a client and server. This type of authentication requires the user to provide a unique username and password in order to access a protected resource. Implementing basic authentication is necessary to prevent unauthorized access to sensitive data and maintain the security of a web application.

## How To: Sending an HTTP request with basic authentication using C#

To send an HTTP request with basic authentication using C#, we can use the `HttpClient` class available in the `System.Net.Http` namespace. The `HttpClient` class provides methods to send an HTTP request to a specific URI, and we can use the `HttpClientHandler` class to add basic authentication to the request.

Let's take a look at the following code snippet to see how we can send an HTTP request with basic authentication using C#:

```C#
// Initialize HttpClient
HttpClient client = new HttpClient();

// Specify the base address of the request
client.BaseAddress = new Uri("https://www.example.com/api/");

// Create a new instance of HttpClientHandler and set credentials
HttpClientHandler handler = new HttpClientHandler();
handler.Credentials = new NetworkCredential("username", "password");

// Assign the handler to the HttpClient instance
client.Handler = handler;

// Define the request URI and add necessary headers
Uri requestUri = new Uri(client.BaseAddress, "resource");
client.DefaultRequestHeaders.Add("Accept", "application/json");

// Send the request and get the response
HttpResponseMessage response = client.GetAsync(requestUri).Result;

// Read the response and display the status code
string result = response.Content.ReadAsStringAsync().Result;
Console.WriteLine($"Status Code: {response.StatusCode}, Result: {result}");
```

### Sample Output:

```
Status Code: OK, Result: { "id": 1, "name": "John Doe", "email": "johndoe@example.com" }
```

As shown in the code above, we first initialize an instance of `HttpClient` and specify the base address of the request. Then, we create an instance of `HttpClientHandler` and provide the necessary credentials. Next, we assign this handler to the `HttpClient` instance. Finally, we define the request URI and headers, send the request, and read the response.

## Deep Dive: Understanding the process of sending an HTTP request with basic authentication

When a client sends an HTTP request with basic authentication, the server checks for the presence of the `Authorization` header in the request. This header contains the word "Basic" followed by a space and the Base64-encoded username and password. Upon receiving this header, the server decodes the username and password and verifies them with the user database. If the credentials match, the server allows access to the requested resource; otherwise, it returns an error message.

It is important to note that the use of basic authentication is discouraged due to the fact that the username and password are encoded, not encrypted, which makes them easily readable by anyone who intercepts the request. It is recommended to use a more secure method of authentication, such as OAuth or JSON Web Tokens, instead.

## See Also

For more information on sending HTTP requests with basic authentication using C#, check out the following resources:

- [MSDN - HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [MSDN - HttpClientHandler Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclienthandler)
- [MDN - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Basic Authentication in Web API](https://www.c-sharpcorner.com/article/basic-authentication-in-web-api/)