---
date: 2024-01-20 18:02:30.095261-07:00
description: "Sending an HTTP request with basic authentication involves tossing a\
  \ username and password into a request to a server to prove you're allowed in.\u2026"
lastmod: 2024-02-19 22:05:18.214977
model: gpt-4-1106-preview
summary: "Sending an HTTP request with basic authentication involves tossing a username\
  \ and password into a request to a server to prove you're allowed in.\u2026"
title: Sending an HTTP request with basic authentication
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves tossing a username and password into a request to a server to prove you're allowed in. Programmers do this to interact with APIs or web services that are locked behind a user-credentials gate.

## How to:

Here's how you get Python to chat with a server using Basic Auth.

```Python
import requests
from requests.auth import HTTPBasicAuth

# Replace with your actual credentials and the API endpoint you're hitting
username = 'cooluser'
password = 'supersecretpassword'
url = 'https://api.someservice.com/data'

response = requests.get(url, auth=HTTPBasicAuth(username, password))

# Check out what we got back
print(response.status_code)
print(response.json())  # assuming the response is in JSON format
```

Output might look like this if things went smooth:

```
200
{'data': 'Your secret stuff!'}
```

But if you goofed up the creds:

```
401
```

That's a no-entry sign right there.

## Deep Dive

Historically, HTTP Basic Auth is as old-school as it gets for web security, a simple way to do the secret handshake with a website. It's not very secure on its own because it sends credentials in plain text, just base64 encoded – not encrypted. Always use HTTPS to keep the credentials from being as easy to snag as candy from a baby.

There are more secure alternatives, like Digest Access Authentication where the password is never sent plain over the network. OAuth is another big one, especially for APIs today. It's more like issuing a temporary VIP pass than showing ID every time.

Under the hood, the `requests` library is encoding your username and password and slapping them into an `Authorization` header formatted like `Basic base64encodedcredentials`. The server decodes this header, checks your creds, and if you're legit, gives you access.

## See Also

- The official `requests` library docs give you the lowdown on auth and more: https://docs.python-requests.org/en/latest/
- `http.client` for those who want to roll without a third-party library: https://docs.python.org/3/library/http.client.html
- Real Python dives into HTTP basics and Python: https://realpython.com/python-requests/
