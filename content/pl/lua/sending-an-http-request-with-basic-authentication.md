---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem w Lua oznacza wysłanie danych na serwer, który wymaga poufnej informacji, jak login i hasło, przed udostępnieniem swojej zawartości. Programiści to robią, aby uzyskać dostęp do chronionych danych i funkcji oferowanych przez serwer.

## Jak to zrobić:

Używając biblioteki luasocket i luasec, można to zrobić w ten sposób:

```Lua
http = require("socket.http")
ltn12 = require("ltn12")

auth = "Basic " .. (mime.b64("login:password")) 

body, code, headers, status = http.request
{
  url = "http://example.com/rest/api/2/search?jql=assignee=demo",
  headers = 
  { 
    ["Authorization"] = auth,
    ["Content-Type"] = "application/json"
  },
  sink = ltn12.sink.file(io.stdout)
}

print("\nResponse data:\n" .. body)
print("\nCode: ", code) 
print("\nHeaders: ", headers, "\nStatus: ", status)
```

## Głębsze Zanurzenie:

Wysyłanie żądań HTTP z podstawowym uwierzytelnianiem ma swoje korzenie w standardach protokołu HTTP stworzonych przez IETF. Jest to jedna z wielu metod uwierzytelniania uwzględnionych w specyfikacji HTTP - inne obejmują Digest Authentication czy OAuth.

W tym kontekście, alternatywą dla tej metody może być użycie innych bibliotek Lua, jak luajit-request czy lua-http. Decyzja o wyborze zależy od wielu czynników, takich jak wymagany poziom bezpieczeństwa, wygoda użycia czy wydajność.

Szczegółowo, podczas wysyłania żądania HTTP z uwierzytelnianiem, twoje dane uwierzytelniające są kodowane do formatu base64 i dołączane do nagłówka żądania. Gdy serwer odbiera żądanie, dekoduje informacje uwierzytelniające i sprawdza, czy są poprawne.

## Zobacz Także:

1. Dokumentacja biblioteki LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
2. Dokumentacja biblioteki LuaSec: https://github.com/brunoos/luasec
3. Protokół uwierzytelniania HTTP Basic: https://pl.wikipedia.org/wiki/Podstawowy_schemat_uwierzytelniania_HTTP
4. Biblioteka luajit-request: https://github.com/LPGhatguy/luajit-request
5. Biblioteka lua-http: https://github.com/daurnimator/lua-http