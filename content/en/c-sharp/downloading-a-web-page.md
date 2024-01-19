---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage is simply retrieving its data from the server where it's stored. This can be done to backup data, screen data, or to analyse the HTML structure. 

## How To:

Let's get to work. Here's a simple example using `HttpClient`:

```csharp
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static readonly HttpClient httpClient = new HttpClient();

    public static async Task Main()
    {
        string webpageUrl = "http://example.com";
        try
        {
            string pageContent = await httpClient.GetStringAsync(webpageUrl);
            Console.WriteLine(pageContent);
        }
        catch (Exception e)
        {
            Console.WriteLine($"Could not download the web page: {e.Message}");
        }
    }
}
```
Run the program and you'll see the HTML content of the specified webpage.

## Deep Dive

Downloading web pages programmatically goes back to the beginning of the web. Despite its old-school nature, it's still quite relevant, especially with the rise of web scraping and data analysis.

Alternative methods exist. For example, `WebClient.DownloadStringTaskAsync` is a simpler method, but `HttpClient` gives you more flexibility and power with headers, cookies, and HTTP methods like POST.

A word of caution, though. While downloading a webpage seems trivial, performance and error handling matters. Be sure to manage connections properly and always close them when done, even when an error occurs. Retries and time-outs are not just academic concerns - you might have to handle them in real life. 

Also, always respect the robots.txt file, and be mindful of a website's terms of service.

## See Also

Further Reading:

1. [MSDN Documentation on HttpClient.GetStringAsync Method](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient.getstringasync)

2. [Web Scraping with C#](http://goalkicker.com/CSharpBook/CSharpNotesForProfessionals334.pdf)

3. [WebClient vs HttpClient vs HttpWebRequest](https://www.darkcoding.net/software/webclient-vs-httpclient-vs-httpwebrequest/) 

4. [How To Handle Errors In HttpClient](https://www.talkingdotnet.com/how-to-handle-exceptions-in-httpclient/)