---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Lua: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawową autentykacją to proces, w którym programista wysyła żądanie HTTP z dodanymi informacjami o uwierzytelnieniu w nagłówku. Używa się tego do zdalnego dostępu do zasobów, które wymagają uwierzytelnienia, takich jak API lub strony internetowe. Programiści to robią, aby dodać warstwę bezpieczeństwa do swoich aplikacji i zapobiec nieautoryzowanemu dostępowi.

## Jak to zrobić:

```Lua
-- przykład wysyłania żądania HTTP z podstawową autentykacją w Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- ustawienie danych uwierzytelniających
local user = "nazwa użytkownika"
local password = "hasło"

-- przygotowanie nagłówka z danymi uwierzytelniającymi
local credentials = user .. ":" .. password
local encodedAuth = "Basic " .. mime.b64(credentials)

-- przygotowanie i wysłanie żądania HTTP
local response = {}
http.request({
    url = "https://example.com/api",
    headers = {authorization = encodedAuth},
    sink = ltn12.sink.table(response)
})

-- wyświetlenie ciała odpowiedzi
print(response[1])
```

Otrzymamy odpowiedź w formacie JSON zawierającą informacje z API.

## Deep Dive:

1. Historyczne tło: Autoryzacja HTTP została wprowadzona w 1994 roku, aby zapewnić bezpieczny dostęp do informacji w Internecie. Podstawowa autentykacja, która jest częścią tej metodologii, jest najprostszą i najmniej bezpieczną formą uwierzytelnienia.
2. Alternatywy: Istnieje wiele innych metod uwierzytelniania, takich jak uwierzytelnianie z wykorzystaniem tokenów, który jest bardziej bezpieczny niż podstawowa autentykacja.
3. Szczegóły implementacji: Podstawowa autentykacja jest realizowana przez dodanie nagłówka "Authorization" do żądania HTTP, zawierającego dane uwierzytelniające w formacie "nazwa użytkownika:hasło". Następnie te dane są kodowane do formatu Base64 i przesyłane w pliku nagłówkowym.

## Zobacz również:

1. [Dokumentacja Lua o modułach HTTP i LTN12](http://w3.impa.br/~diego/software/luasocket/http.html)
2. [Informacje o bezpieczeństwie uwierzytelniania HTTP](https://www.owasp.org/index.php/Basic_Authentication)
3. [Porównanie różnych metod uwierzytelniania HTTP](https://stackoverflow.com/questions/24204459/whats-the-difference-between-basic-and-digest-authentication-in-http)