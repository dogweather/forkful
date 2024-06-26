---
date: 2024-01-20 18:00:29.345916-07:00
description: 'How to: Here''s the straightforward deal for firing off a simple GET
  request.'
lastmod: '2024-03-13T22:45:00.281201-06:00'
model: gpt-4-1106-preview
summary: Here's the straightforward deal for firing off a simple GET request.
title: Sending an HTTP request
weight: 44
---

## How to:
Here's the straightforward deal for firing off a simple GET request:

```PowerShell
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get
Write-Output $response
```

And, if you're looking to POST some info:

```PowerShell
$body = @{
    'name' = 'Jane Doe'
    'occupation' = 'Space Ranger'
}

$response = Invoke-RestMethod -Uri 'https://api.example.com/users' -Method Post -Body ($body | ConvertTo-Json)
Write-Output $response
```

Sample output:

```
name         occupation
----         ----------
Jane Doe     Space Ranger
```

## Deep Dive:
Sending an HTTP request harks back to the dawn of web development. You're engaging in a dialog with the web in its native tongue, HTTP. PowerShell's `Invoke-RestMethod` cmdlet is the tool of choice here. Before `Invoke-RestMethod`, `Invoke-WebRequest` was the go-to, and it's still around for more detailed responses.

You've got alternatives like `curl` or .NET's `HttpClient` class if you're feeling adventurous. When using `Invoke-RestMethod`, remember it's a wrapper around .NET's `HttpClient` classes and methods, offering simplicity but trading off some lower-level control.

Implementation-wise, remember HTTP requests come with various methods like `GET`, `POST`, `PUT`, etc. Customize headers with `-Headers`, and manage time-outs and authentication with extra params as needed. Always sanitize inputs if you're using user-generated content to avoid injection attacks.

## See Also:
- [PowerShell's About Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [`Invoke-WebRequest` details](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Understanding REST APIs](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [`.NET HttpClient` Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
