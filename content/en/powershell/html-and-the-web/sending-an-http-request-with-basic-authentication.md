---
date: 2024-01-20 18:02:23.036437-07:00
description: 'How to: Here''s how you ask a server nicely for data with a ''please''
  in the form of basic authentication.'
lastmod: '2024-03-13T22:45:00.283766-06:00'
model: gpt-4-1106-preview
summary: Here's how you ask a server nicely for data with a 'please' in the form of
  basic authentication.
title: Sending an HTTP request with basic authentication
weight: 45
---

## How to:
Here's how you ask a server nicely for data with a 'please' in the form of basic authentication:

```PowerShell
# Prepping the credentials
$user = 'YourUsername'
$pass = 'YourPassword'
$pair = "$($user):$($pass)"
$encodedCreds = [System.Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($pair))

# Setting up the headers
$headers = @{
    Authorization = "Basic $encodedCreds"
}

# The URL you're knocking on
$url = 'https://api.example.com/data'

# Now, let's make the call
$response = Invoke-RestMethod -Uri $url -Method Get -Headers $headers

# Output the results
$response
```

Sample output might look like this, assuming the response is in JSON format:

```json
{
    "name": "John Doe",
    "email": "john@example.com"
}
```

## Deep Dive
Basic auth is old-school, tracing back to the early days of the internet where everyone knew everyone. While still used, it's not super secure on its own - it's like sending your secret club password on a postcard. Nowadays, we usually send it over HTTPS to encrypt it, which is like putting that postcard in a locked box.

Alternatives? Plenty. You've got API keys, OAuth, bearer tokens... the list goes on. Each comes with its own handshakes and secret words.

Implementation-wise, with PowerShell, you're converting your username and password to a format that the HTTP protocol can understand – base64. But remember, base64 isn't encryption; it's just text toying with a disguise. Any snooper can reveal it unless it's sent over HTTPS.

## See Also
- [Invoke-RestMethod Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [HTTP Basic Access Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Understanding Base64 Encoding](https://en.wikipedia.org/wiki/Base64)
- [Info on HTTPS Encryption](https://en.wikipedia.org/wiki/HTTPS)
