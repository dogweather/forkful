---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? 
Sending an HTTP request with basic authentication is using a simple authentication scheme of the HyperText Transfer Protocol (HTTP) to protect data transfer between systems. Programmers do it to ensure that only authorized individuals can access certain data or features of a web application.

## How to:
Below is a step-by-step guide on sending an HTTP request with basic authentication.

```PowerShell
# Supply your credentials
$Username = 'YourUsername'
$Password = 'YourPassword'
 
# Create a credentials object
$SecPass = ConvertTo-SecureString $Password -AsPlainText -Force
$Cred = New-Object System.Management.Automation.PSCredential ($Username, $SecPass)
 
# Define the URL to send the request to
$URL = 'http://your-url.com'
 
# Use Invoke-WebRequest to send the HTTP request
$Response = Invoke-WebRequest -Uri $URL -Method Get -Credential $Cred
 
# Print the response
$Response.Content
```

After running this script, you should see an output similar to this:

```PowerShell
<!DOCTYPE html>
<html lang='en'>
<head>
  <title>Your Website Title</title>
  <!-- other meta tags -->
</head>
<body>
  <!-- your website content -->
</body>
</html>
```
## Deep Dive
Basic authentication is a part of HTTP protocol since its inception in 1991. It simply transmits the credentials in clear text, base64 encoded. This doesn't provide strong security as it can be decoded easily.

If you're looking for alternatives, you might want to consider Digest Access Authentication, an extension to HTTP that applies MD5 cryptographic hashing with usage of nonces to prevent replay attacks. Another alternative is token-based authentication which uses tokens for clients to enter their information in a secure way.

In the PowerShell example, we're using `Invoke-WebRequest` cmdlet which sends an HTTP or HTTPS request to a RESTful web service. `-Uri` parameter specifies the Uniform Resource Identifier (URI) of the Internet resource to which the web request is sent. `-Method` parameter represents the method used for web request. `-Credential` contains the credential object that authorizes the connection to the remote computer. 

## See Also
For further reading, do check out these sources:

- [About Authentication](https://docs.microsoft.com/en-us/aspnet/core/security/authentication/?view=aspnetcore-5.0)
- [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)