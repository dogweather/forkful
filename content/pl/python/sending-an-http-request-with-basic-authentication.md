---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Python: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z podstawową autoryzacją może być konieczne, gdy potrzebujemy uzyskać dostęp do chronionego zasobu na stronie internetowej lub serwerze. W niektórych przypadkach, aby uzyskać dostęp do danego zasobu, musimy udowodnić swoją tożsamość poprzez podanie nazwy użytkownika i hasła.

## Jak to zrobić

W języku Python istnieje wiele bibliotek, które umożliwiają nam wysyłanie żądań HTTP z podstawową autoryzacją. Jedną z najpopularniejszych jest biblioteka `requests`, która pozwala na prosty i intuicyjny sposób na komunikację z serwerami i pobieranie danych. Aby wysłać żądanie z podstawową autoryzacją, należy użyć parametru `auth` i przekazać do niego obiekt klasy `HTTPBasicAuth`, przekazując nazwę użytkownika i hasło jako jego argumenty. Poniżej przedstawiony jest przykładowy kod:

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get("http://example.com/protected-resource", auth=HTTPBasicAuth('nazwa_użytkownika', 'hasło'))
print(response.text)  # wyświetlenie zawartości odwołanego zasobu
```

Przykładowy wynik:

`Strona zabezpieczona za pomocą podstawowej autoryzacji!`

Zauważ, że w tym przypadku podajemy dane do uwierzytelnienia bezpośrednio w kodzie, co nie jest zalecane ze względów bezpieczeństwa. W praktyce, warto przechowywać je w zmiennych zdefiniowanych w osobnym pliku konfiguracyjnym, lub pobierać je z zewnętrznego źródła.

## Deep Dive

Podczas wysyłania żądania z podstawową autoryzacją, wysyłane są nagłówki `Authorization`, zawierające nazwę użytkownika i zaszyfrowane hasło. Warto zauważyć, że podczas korzystania z podstawowej autoryzacji, hasło jest szyfrowane jedynie przy użyciu kodowania Base64, co nie jest metodą bezpieczną. Dlatego też zaleca się stosowanie bardziej zaawansowanych metod uwierzytelniania, jak np. autoryzacja żetonowa.

## Zobacz także

1. Dokumentacja biblioteki `requests`: https://requests.readthedocs.io/en/latest/
2. Poradnik o podstawowej autoryzacji w protokole HTTP: https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication