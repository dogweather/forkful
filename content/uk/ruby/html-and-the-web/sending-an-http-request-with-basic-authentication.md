---
date: 2024-01-20 18:02:21.873727-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Here's the simplest way to send an HTTP request with basic authentication\
  \ using Ruby's `net/http` library."
lastmod: '2024-04-05T21:53:50.257273-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ Here's the simplest way to send an HTTP request with basic authentication using\
  \ Ruby's `net/http` library."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## How to: (Як це зробити:)
Here's the simplest way to send an HTTP request with basic authentication using Ruby's `net/http` library:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/resource')
request = Net::HTTP::Get.new(uri)
request.basic_auth('user', 'password')

response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

puts response.body
```

Sample output might look like the content of the requested resource:

```Ruby
"Here's your protected resource content!"
```

## Deep Dive (Поглиблено):
Ruby has supported HTTP requests for a long time. Basic auth isn't the latest trend (more secure alternatives exist, like OAuth), but it remains relevant for simplicity and legacy systems. Behind the scenes, Ruby encodes the credentials and adds an `Authorization` header to the HTTP request. It's not super secure (think plain text over HTTP), so use HTTPS to encrypt the transmission. Also, consider storing credentials securely and not hard-coding them.

## See Also (Дивіться також):
- `HTTP` gem for a more feature-rich and friendly API: [https://github.com/httprb/http](https://github.com/httprb/http)
