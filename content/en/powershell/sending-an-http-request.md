---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# PowerShell HTTP Requests: Tips, Tricks, and Deep Dives

## What & Why?
Ever needed to speak with a web server directly? That's sending an HTTP request. Programmers use them to interact with APIs, download files, and so on. Who needs a browser anyway!

## How To:
Looking to interact with a web server using PowerShell? Use Invoke-WebRequest or Invoke-RestMethod. Here's an example:

```PowerShell
# Get HTTP response
$response = Invoke-WebRequest -Uri "http://example.com"

# Display the status
$response.StatusCode
```
Want to get JSON results? Try this:

```PowerShell
# Get HTTP response from JSON endpoint
$response = Invoke-RestMethod -Uri "http://example.com/api/endpoint"

# Display the result
$response
```

## Deep Dive
Back in the day, we used WebClient or HttpWebRequest classes to send HTTP requests in .NET. However, unlike these .NET classes, PowerShell commands deal with all the complexity for you - now ain't that nice? 

An alternative to PowerShell, you might use cURL in *nix systems. But, why move to *nix and learn cURL when PowerShell can do the same for you?

Under the hood, both Invoke-WebRequest and Invoke-RestMethod use the .NET HttpClient class. The difference? Invoke-RestMethod automatically converts JSON responses into PowerShell objects.

## See Also
- [PowerShell docs: Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [PowerShell docs: Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7)