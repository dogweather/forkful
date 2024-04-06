---
date: 2024-01-20 18:00:30.101099-07:00
description: "How to: \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: Ruby's standard library, Net::HTTP, is simple to use for sending requests."
lastmod: '2024-04-05T22:38:49.077497-06:00'
model: gpt-4-1106-preview
summary: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Ruby's\
  \ standard library, Net::HTTP, is simple to use for sending requests."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## How to:
Як це зробити:

Ruby's standard library, Net::HTTP, is simple to use for sending requests:

```ruby
require 'net/http'
require 'uri'

uri = URI('https://api.example.com/items')
response = Net::HTTP.get(uri)

puts response
```

Sample output:

```
[{"id":1,"name":"Apple"},{"id":2,"name":"Orange"}]
```

Post request with form data:

```ruby
require 'net/http'
require 'uri'

uri = URI('https://api.example.com/items')
request = Net::HTTP::Post.new(uri)
request.set_form_data({'name' => 'Banana'})

response = Net::HTTP.start(uri.hostname, uri.port, use_ssl: uri.scheme == 'https') do |http|
  http.request(request)
end

puts response.body
```

Assuming the API acknowledges the POST request:

```
{"id":3,"name":"Banana","status":"created"}
```

## Deep Dive
Поглиблений Аналіз:

Ruby's Net::HTTP module has been around since the 1.x days, constantly evolving. It became friendlier with the introduction of methods like `Net::HTTP.get` and wrappers like OpenURI. It's basic, but it works.

Alternatives? You bet. Many prefer gems like 'httparty' or 'rest-client' for syntactic sugar. They're more intuitive and feature-packed.

Implementation details? Using `Net::HTTP.start` helps manage connections more effectively. SSL? Set `use_ssl: true` and always verify certificates to avoid security risks.

## See Also
Дивіться Також:

- Ruby Net::HTTP documentation: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- httparty gem: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- rest-client gem: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
- Ruby security best practices: [https://brakemanscanner.org/docs/warning_types/ssl_verification_bypass/](https://brakemanscanner.org/docs/warning_types/ssl_verification_bypass/)
