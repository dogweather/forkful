---
date: 2024-01-20 18:02:19.055460-07:00
description: "How to: (Jak to zrobi\u0107:) Wysy\u0142anie \u017C\u0105dania HTTP\
  \ z uwierzytelnianiem podstawowym nie jest nowo\u015Bci\u0105. Wczesne przegl\u0105\
  darki internetowe ju\u017C to obs\u0142ugiwa\u0142y.\u2026"
lastmod: '2024-04-05T22:50:49.862546-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wysy\u0142anie \u017C\u0105dania HTTP z uwierzytelnianiem\
  \ podstawowym nie jest nowo\u015Bci\u0105."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## How to: (Jak to zrobić:)
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Kodowanie danych logowania
local username = "jan_kowalski"
local password = "bezpieczneHaslo123"
local auth = "Basic " .. mime.b64(username .. ":" .. password)

-- Przygotowanie żądania
local response_body = {}
local request_body = "param=value"
local res, code, response_headers = http.request{
    url = "http://example.com/api/resource",
    method = "POST",
    headers = {
        ["Authorization"] = auth,
        ["Content-Type"] = "application/x-www-form-urlencoded",
        ["Content-Length"] = tostring(#request_body)
    },
    source = ltn12.source.string(request_body),
    sink = ltn12.sink.table(response_body)
}

-- Wyświetlenie odpowiedzi
if res then
    print("Odpowiedź serwera:", table.concat(response_body))
else
    print("Błąd:", code)
end
```
Sample output:
```
Odpowiedź serwera: {"status": "success"}
```

## Deep Dive (Dogłębna analiza)
Wysyłanie żądania HTTP z uwierzytelnianiem podstawowym nie jest nowością. Wczesne przeglądarki internetowe już to obsługiwały. Alternatywą dla Basic Authentication jest OAuth, Token Auth czy inne mechanizmy, które mogą oferować większe bezpieczeństwo. Do uwierzytelniania podstawowego używa się zakodowanego w Base64 łańcucha znaków zawierającego login i hasło rozdzielone dwukropkiem, dołączane do nagłówków żądania. Choć łatwe w implementacji, metoda ta jest mniej bezpieczna niż bardziej zaawansowane formy autoryzacji, gdyż może być podatna na ataki typu man-in-the-middle oraz nie zapewnia szyfrowania danych logowania.

## See Also (Zobacz również)
- [LuaSec](https://github.com/brunoos/luasec) – biblioteka do bezpiecznych połączeń SSL dla Lua.
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617) – dokumentacja techniczna opisująca standard uwierzytelniania podstawowego.
