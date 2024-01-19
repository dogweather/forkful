---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page is all about fetching its HTML content using a program. Programmers often do this to automate interactions with websites, gather data, or to test website functionality.

## How to:

To download a web page with PowerShell, we use the `Invoke-WebRequest` cmdlet. At its simplest, the `Invoke-WebRequest` cmdlet accepts a URL and returns the page content. Here's an easy-peasy way to grab the HTML of a web page.

```PowerShell
$results = Invoke-WebRequest -Uri "https://google.com" # replace with your URL
$results.Content
```

Run this, and what you'll get in `$results.Content` is the HTML of the page at `https://google.com`.

## Deep Dive:

Powershell introduced the `Invoke-WebRequest` cmdlet in v3.0 as a modern method to interact with the web programmatically. It encapsulates much of the complexity of lower-level protocols like HTTP or web scraping libraries.

As alternatives, you could use the `WebClient` or `HttpWebRequest` classes in .NET. These options give you more control but require more code too. For example:

```PowerShell
$webclient = New-Object System.Net.WebClient
$content = $webclient.DownloadString("https://google.com") # replace with your URL
$content
```
This will give you the same HTML content as `Invoke-WebRequest`.

Finally, the `Invoke-WebRequest` cmdlet works by sending an HTTP GET request to a URL and receiving an HTTP response back. The HTML of the page lives in the body of the HTTP response, and that's what we get in `$results.Content`.

## See Also:

For more on `Invoke-WebRequest`, check its [official documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7).

For a deeper dive into HTTP concepts relevant here, check [Mozilla's HTTP guide](https://developer.mozilla.org/en-US/docs/Web/HTTP).