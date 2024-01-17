---
title:                "Wysyłanie żądania http z podstawową autoryzacją."
html_title:           "Ruby: Wysyłanie żądania http z podstawową autoryzacją."
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego? 
Wysyłanie żądania HTTP z podstawową autoryzacją, to po prostu sposób na uwierzytelnienie żądania do serwera. Programiści robią to, aby zapewnić bezpieczny dostęp do zasobów i chronić je przed niepowołanym dostępem.

## Jak to zrobić:
```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("https://example.com/")
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true

request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth("username", "password")

response = http.request(request)

puts response.code
```
Output:
200

## W parę słów więcej:
- Podstawowa autoryzacja została wprowadzona w protokole HTTP w 1999 roku.
- Alternatywą dla tej metody jest wykorzystanie tokenów uwierzytelniających lub OAuth.
- Implementację można znaleźć w bibliotece Net::HTTP, która jest dostępna w standardowej bibliotece Ruby.

## Zobacz także:
- [Net::HTTP documentation](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP Authentication: Basic and Digest Access Authentication](https://www.ietf.org/rfc/rfc2617.txt)