---
title:                "Sending an http request with basic authentication"
html_title:           "PowerShell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a way for a programmer to securely communicate with a server over the internet. It involves including a username and password in the request, which is then verified by the server before granting access. This is a common method used to protect sensitive data and ensure only authorized users have access.

## How to:

To send an HTTP request with basic authentication in PowerShell, you can use the `Invoke-WebRequest` cmdlet. Here is an example of how to send a request to a website that requires basic authentication:

```PowerShell
$request = Invoke-WebRequest -Uri "https://example.com" -Credential (Get-Credential)
$request.Content
```

The `Invoke-WebRequest` cmdlet will prompt you for a username and password, which you can provide through the `Get-Credential` cmdlet. The output of the request can be accessed through the `.Content` property.

## Deep Dive:

HTTP basic authentication has been around since the early days of the World Wide Web and is still widely used today. Although it is a simple and straightforward method of authentication, it does have some drawbacks. For example, the username and password are sent in plain text, making it vulnerable to interception.

There are alternative methods of authentication, such as OAuth and token-based authentication, that provide a more secure way of communicating with a server. However, basic authentication is still commonly used due to its simplicity and compatibility with older systems.

When sending an HTTP request with basic authentication, the username and password are encoded and included in the request header. The server then decodes this information and verifies it before allowing access. It is important to note that this method is only effective over an encrypted connection, such as HTTPS.

## See Also:

- [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
- [Understanding HTTP Basic Authentication](https://www.digitalocean.com/community/tutorials/understanding-http-basic-authentication)