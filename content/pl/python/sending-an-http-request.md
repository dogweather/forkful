---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Wysyłanie żądań HTTP polega na komunikacji między klientem (tj. Twoim komputerem) i serwerem sieciowym. Programiści robią to aby pobierać zmienne dane z sieci, takie jak aktualizacje pogody, wiadomości, informacje finansowe i wiele innych.

## Jak to zrobić:

Python ma wiele bibliotek do pracy z HTTP, ale jednym z najprostszych w użyciu jest `requests`. Sprawdźmy to:

```Python
import requests

response = requests.get('https://api.github.com')
print(response.json())
```

Po uruchomieniu tego kodu, zobaczysz wynik, który jest zwracany w formacie JSON przez API GitHuba.

## Głębsze zrozumienie

Pierwotnie, protokół HTTP został stworzony w 1991 roku i stał się podstawą komunikacji w Internecie. W Pythonie, do wysyłania zapytań HTTP można używać różnych pakietów, takich jak `httplib`, `httplib2`, `treq`, czy `aiohttp`. Jednakże biblioteka `requests` stała się popularna dzięki swojej prostocie i wyrafinowanemu API. Wysyłanie zapytań HTTP jest operacją blokującą, co oznacza, że ​​cały program czeka, aż serwer odpowiedzi na zapytanie. W związku z tym, warto zwrócić uwagę na zarządzanie wydajnością podczas tworzenia aplikacji opartych na żądaniu HTTP.

## Zobacz także

- Dokumentacja 'requests': http://docs.python-requests.org/en/master/
- Informacje o protokole HTTP: https://www.w3.org/Protocols/
- Alternatywne biblioteki do wysyłania żądań HTTP w Pythonie:
  - `httplib2`: https://github.com/httplib2/httplib2
  - `treq`: https://github.com/twisted/treq
  - `aiohttp`: https://github.com/aio-libs/aiohttp