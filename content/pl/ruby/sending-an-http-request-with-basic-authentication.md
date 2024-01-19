---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to proces, w którym programista przesyła dane logowania (takie jak nazwa użytkownika i hasło) w nagłówku żądania HTTP, aby uzyskać dostęp do zasobów po stronie serwera. Robi się to głównie po to, aby zdobyć uprawnienia do wykonywania pewnych operacji na stronie internetowej lub w aplikacji.

## Jak to zrobić:

To jest przykładowy kod w Ruby, który ilustruje, jak wysłać żądanie HTTP z podstawowym uwierzytelnieniem:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse('http://example.com/path')
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth('username', 'password')
response = http.request(request)
puts response.body
```

Gdzie 'username' to nazwa użytkownika, a 'password' to hasło zasobu, do którego chcesz uzyskać dostęp. Po uruchomieniu tego kodu, wydrukowany zostanie odpowiedź ciała żądania.

## Głębsze zanurzenie:

Pomysł wysyłania żądań HTTP z uwierzytelnieniem sięga początków internetu. Na początku uwierzytelnianie było prostym sposobem sprawdzenia, czy użytkownik posiada prawidłowe konto i hasło należące do aplikacji.

Alternatywą dla podstawowego uwierzytelnienia może być uwierzytelnianie typu Digest lub uwierzytelnianie przez token Bearer. Oba zapewniają wyższy poziom bezpieczeństwa, jednak są bardziej skomplikowane do implementacji.

Rzecz, którą warto zauważyć podczas wysyłania żądań HTTP z podstawowym uwierzytelnieniem, jest to, że informacje o logowaniu są przesyłane jako tekst jawny w nagłówku żądania. To oznacza, że jeśli połączenie nie jest zabezpieczone (tj. nie jest to połączenie HTTPS), te informacje mogą być łatwo przechwycone i odczytane przez osoby postronne.

## Zobacz też:

1. [Dokumentacja Ruby Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
2. [HTTP Authentication: Basic and Digest Access Authentication (IETF)](https://tools.ietf.org/html/rfc2617)
3. [Uwierzytelnianie bearer token w Ruby](https://www.devglan.com/ruby-on-rails/ruby-http-client-get-post).