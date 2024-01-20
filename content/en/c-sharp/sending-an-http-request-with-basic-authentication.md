---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Basic Authentication with HTTP requests gives us a way to protect our data by including a Username and Password in the request. As developers, we implement this in our applications when we want to interact with protected APIs or services.

## How To:

Here's a quick and simple example:
```C#
using System;
using System.Net.Http;
using System.Text;

class Program
{
    static void Main()
    {
        var httpClient = new HttpClient();
        var byteArray = Encoding.ASCII.GetBytes("myusername:mypassword");
        httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));
        var response = httpClient.GetAsync("http://my_secure_api.com").GetAwaiter().GetResult();

        Console.WriteLine(response.StatusCode);
    }
}
```
In this snippet, "myusername:mypassword" are your credentials (replace them with your actual ones) and "http://my_secure_api.com" is the URL of the secured API you want to access. The status of the request will be displayed.

## Deep Dive

**Historical Context**: Basic Authentication is one of the simplest methods to enforce access controls to web resources, and it's been there since the early days of HTTP. However, because it involves transmitting plain credentials, it should always be used along with HTTPS.

**Alternatives**: There are many modern alternatives available for authentication, such as OAuth, JWT (JSON Web Token), and API Key-based authentication. Each comes with its strengths and weaknesses and should be chosen according to the requirements.

**Implementation Details**: Note that, by using the HttpClient `DefaultRequestHeaders.Authorization` property, we set the authorization header for all subsequent requests made with this HttpClient instance. 

## See Also

[HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0) from Microsoft's official .NET docs offers key details about the HttpClient class and its methods.

[Different forms of Authentications](https://www.loginradius.com/engineering/blog/different-forms-of-website-authentication/) is a good resource for understanding alternatives for Basic Authentication.