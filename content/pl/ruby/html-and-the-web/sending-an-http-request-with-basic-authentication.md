---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
aliases:
- /pl/ruby/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:37.871553-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Wysyłanie żądania HTTP z podstawową autoryzacją to proces dodawania nagłówka autoryzacyjnego do żądania HTTP, aby potwierdzić, że mamy dostęp. Programiści robią to, by bezpiecznie komunikować się z chronionymi zasobami API.

## How to: (Jak to zrobić:)

Ruby używa `net/http` do wysyłania żądań HTTP. Dołączanie podstawowej autoryzacji jest proste:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/secrets')
username = 'foo'
password = 'bar'

request = Net::HTTP::Get.new(uri)
request.basic_auth(username, password)

response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

puts response.body
```

Jeśli wszystko jest poprawne, odpowiedź będzie zawierać dane, do których dostęp uzyskaliśmy.

## Deep Dive (W Głębi Tematu)

Podstawowa autoryzacja HTTP to stara metoda (RFC 7617), niezbyt bezpieczna – używa base64, a nie szyfrowania. Alternatywy to Digest Access Authentication lub bardziej bezpieczne tokeny, jak OAuth.

Podstawowa autoryzacja w Ruby jest prosta – dodaje zakodowany login i hasło do nagłówka żądania. W realnych aplikacjach zaleca się użycie HTTPS, by zabezpieczyć dane uwierzytelniające.

## See Also (Zobacz również)

- Dokumentacja Basic Authentication w RFC 7617: [The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- Wprowadzenie do autoryzacji HTTP: [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
