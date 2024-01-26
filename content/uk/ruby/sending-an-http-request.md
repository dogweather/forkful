---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T18:00:30.101099-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Суть та Причини?

Sending HTTP requests is how your Ruby program talks to the web; it's like asking a question and awaiting the answer. Programmers do this to retrieve data, submit forms, or interact with APIs – essential for feature-rich applications.

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
