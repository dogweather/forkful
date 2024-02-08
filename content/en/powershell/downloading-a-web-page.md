---
title:                "Downloading a web page"
aliases:
- en/powershell/downloading-a-web-page.md
date:                  2024-01-20T17:44:41.290845-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page means snagging its contents via the web. Coders do this for web scraping, offline viewing, or automating interactions with websites.

## How to:
Here's the magic spell for fetching a web page using PowerShell. We'll harness `Invoke-WebRequest`.

```PowerShell
# Grab the content of example.com
$response = Invoke-WebRequest -Uri "http://example.com"

# Here's what you got
$response.Content
```

Sample output: 

```PowerShell
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
    <!-- and so on -->
</head>
...
</html>
```

You could be after just text, no HTML tags. Let's do that:

```PowerShell
# Just the text, please
$response.ParsedHtml.body.innerText
```

## Deep Dive
Once upon a time, PowerShell didn’t have the cool `Invoke-WebRequest` cmdlet. Coders would use the .NET `System.Net.WebClient` class or resort to external tools. Now, it's all built-in, simplifying tasks for us all.

`Invoke-WebRequest` offers more than just content. Headers, status, and session info – it’s all there. If you're playing with APIs, you'll love `Invoke-RestMethod` as a focused alternative.

Under the hood, these cmdlets rely on the heavyweight .NET HttpClient class, packing reliability and extensive functionality.

And, if you're getting impatient waiting for that web page to download, `Invoke-WebRequest` supports asynchronous operations too. However, that's a topic for another day.

## See Also
- The [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- More about [Invoke-RestMethod for API interactions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- A [PowerShell GitHub repository](https://github.com/PowerShell/PowerShell) for the curious coders who like to peek under the hood.
