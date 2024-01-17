---
title:                "Wysyłanie żądania http"
html_title:           "Python: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co & Po co?

Wysyłanie żądań HTTP jest podstawowym sposobem, w jaki programiści komunikują się z siecią. Za pomocą tych żądań można pobierać lub wysyłać dane do internetowych zasobów, takich jak strony internetowe lub API. Jest to niezbędne do tworzenia aplikacji internetowych, które łączą się z serwerami lub innymi zasobami w sieci.

## Jak to zrobić:

```Python
# Wykonanie żądania GET za pomocą biblioteki requests
import requests

response = requests.get('https://www.example.com')

# Sprawdzenie kodu odpowiedzi
if response.status_code == 200:
	# Wyświetlenie pobranej zawartości
	print(response.text)
else:
	print("Błąd podczas pobierania strony")
```

```Python
# Wykonanie żądania POST z wykorzystaniem danych w formacie JSON
import requests

data = {'username': 'John', 'password': '12345'}

response = requests.post('https://www.example.com/login', json=data)

# Sprawdzenie kodu odpowiedzi
if response.status_code == 200:
	# Wyświetlenie komunikatu zwrotnego
	print(response.json()['message'])
else:
	print("Błąd podczas logowania")
```

## Głębszy zanurzenie:

Wysyłanie żądań HTTP ma swoje początki w protokole HTTP, który został opracowany w latach 80. XX wieku. Alternatywnym podejściem do komunikacji z siecią jest wykorzystanie gniazd sieciowych, jednak żądania HTTP są bardziej wygodne i wszechstronne. Istnieje wiele bibliotek w języku Python, które ułatwiają wysyłanie żądań HTTP, takich jak requests, urllib oraz httplib.

## Zobacz również:

Dokumentacja biblioteki requests: https://requests.readthedocs.io/en/master/

Porównanie różnych bibliotek do obsługi żądań HTTP w Pythonie: https://www.pythonforbeginners.com/requests/which-python-http-library-to-use

Wprowadzenie do protokołu HTTP: https://developer.mozilla.org/pl/docs/Web/HTTP