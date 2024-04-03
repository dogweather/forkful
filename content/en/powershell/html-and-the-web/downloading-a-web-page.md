---
date: 2024-01-20 17:44:41.290845-07:00
description: Downloading a web page means snagging its contents via the web. Coders
  do this for web scraping, offline viewing, or automating interactions with websites.
lastmod: '2024-03-13T22:45:00.282906-06:00'
model: gpt-4-1106-preview
summary: Downloading a web page means snagging its contents via the web.
title: Downloading a web page
weight: 42
---

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
