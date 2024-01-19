---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie zapytań HTTP z podstawowym uwierzytelnieniem polega na przekazaniu danych logowania do serwera w celu autoryzacji. Programiści robią to, aby uzyskać dostęp do zasobów dostępnych tylko dla zarejestrowanych lub zalogowanych użytkowników.

## Jak to zrobić:

Python oferuje wiele modułów do pracy z sieciami. Oto przykład, jak to zrobimy za pomocą modułu `requests`.

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://pomocniczy-api.pl', auth=HTTPBasicAuth('uzytkownik', 'haslo'))

print(response.status_code)
```

Przykładowe wyjście:

```Python
200
```

## Głębiej:

**Kontekst historyczny**: Uwierzytelnianie HTTP ma swoje korzenie w protokole HTTP. Wprowadzono je, gdy zorientowano się, że istnieje potrzeba zabezpieczania informacji na stronach internetowych.

**Alternatywy**: Oprócz podstawowego uwierzytelnienia, istnieją inne schematy uwierzytelniania, takie jak uwierzytelnianie Digest lub Bearer. Moduł `requests` obsługuje również te typy uwierzytelnienia.

**Szczegóły implementacji**: Podstawowe uwierzytelnianie HTTP jest proste w użyciu, ale ma pewne słabości. Dlatego zawsze powinniśmy korzystać z połączenia HTTPS, aby chronić dane uwierzytelniające.

## Zobacz również:

- [Dokumentacja modułu requests](https://requests.readthedocs.io/en/latest/)
- [Specyfikacja uwierzytelniania HTTP](https://tools.ietf.org/html/rfc7617)