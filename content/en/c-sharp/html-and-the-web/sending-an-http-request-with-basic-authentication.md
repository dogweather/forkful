---
date: 2024-01-20 18:01:10.612838-07:00
description: "We send an HTTP request with basic authentication to access protected\
  \ resources by including user credentials in the request header. Programmers use\
  \ it\u2026"
lastmod: '2024-03-13T22:45:00.088380-06:00'
model: gpt-4-1106-preview
summary: "We send an HTTP request with basic authentication to access protected resources\
  \ by including user credentials in the request header. Programmers use it\u2026"
title: Sending an HTTP request with basic authentication
weight: 45
---

## What & Why?
We send an HTTP request with basic authentication to access protected resources by including user credentials in the request header. Programmers use it for simple auth systems, mainly where a quick and straightforward solution is suitable.

## How to:
Let's jump straight in with some code. Below is a minimal example using C# to send an HTTP request with basic authentication:

```C#
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using (var client = new HttpClient())
        {
            var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", credentials);

            HttpResponseMessage response = await client.GetAsync("http://yourapi.com/protected");

            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine($"Error: {response.StatusCode}");
            }
        }
    }
}
```
Run this, and if your endpoint and creds are correct, you'll get the resource. If not, you'll see an error status code.

## Deep Dive
Basic Authentication is old, like really old, dating back to the early days of the internet. It's simple: base64-encode "username:password" and slap it onto the `Authorization` header. 

There are alternatives with tighter security: OAuth2, API keys, or JWT tokens. Basic Auth is still around due to its simplicity, but beware it's not encrypted and can be intercepted if not used over HTTPS.

When you use this method, keep in mind:
- Always use HTTPS to protect the credentials in transit.
- It's a bit like leaving your house key under the mat – convenient but vulnerable. So use it for low-risk scenarios.
- Since the credentials are passed with each request, it’s not the most efficient method for busy systems.

## See Also
- [Microsoft's HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Mozilla's Basic Authentication Explanation](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OWASP Authentication Cheat Sheet](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)
