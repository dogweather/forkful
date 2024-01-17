---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Python: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to się robi?
Wysyłanie żądania HTTP z podstawową autoryzacją to sposób na uwierzytelnienie przesyłanych danych. Programiści robią to, aby zapewnić bezpieczny i poufny sposób komunikacji między serwerem a klientem.

## Jak to zrobić:
```
import requests

URL = "https://www.example.com"
USERNAME = "login"
PASSWORD = "password"

response = requests.get(URL, auth=(USERNAME, PASSWORD))
print(response.text)
```
Wyjście:
```
Hello, world!
```

## Głębsza analiza:
Podstawowa autoryzacja HTTP jest jedną z najstarszych metod uwierzytelniania dostępu do sieci. Alternatywami są m.in. uwierzytelnianie oparte na tokenach i digest authentication. Aby wysłać żądanie z podstawową autoryzacją, należy dodać do nagłówka `Authorization` nazwę użytkownika i hasło w formacie `username:password` zaszyfrowane w base64.

## Zobacz także:
- [Dokumentacja biblioteki requests](https://requests.readthedocs.io/en/master/)
- [Autoryzacja HTTP w szczegółach](https://www.ietf.org/rfc/rfc2617.txt)