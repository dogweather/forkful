---
title:                "Pobieranie strony internetowej"
html_title:           "Python: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych jest jedną z często wykonywanych operacji w programowaniu. Może być użyteczne do tworzenia narzędzi do analizowania danych lub do pobierania potrzebnych informacji z sieci. 

## Jak To Zrobić

Pobieranie stron internetowych jest możliwe dzięki użyciu pakietu `requests`, który pozwala na wysyłanie żądań HTTP w prosty sposób.

Przykładowy kod pobrania strony internetowej wygląda następująco:

```Python
import requests

url = 'https://www.example.com'
response = requests.get(url)
print(response.content)
```

W powyższym kodzie, najpierw importujemy pakiet `requests`, a następnie definiujemy zmienną `url` z adresem strony internetowej, którą chcemy pobrać. Następnie używamy metody `get()` z pakietu `requests` i przypisujemy ją do zmiennej `response`. Ostatecznie, korzystając z atrybutu `content`, wyświetlamy pobrany kod HTML strony.

Zamiast korzystać ze zmiennej `url`, możemy również bezpośrednio podać adres strony internetowej w metodzie `get()`, np.: `response = requests.get('https://www.example.com')`.

Wynik działania powyższego kodu będzie wyglądał podobnie do poniższego:

```
b'<!doctype html> // tu znajduje się kod HTML pobranej strony'
```

## Wnikliwe Badanie

Pakiet `requests` oferuje wiele dodatkowych opcji, pozwalając na bardziej zaawansowane pobranie stron internetowych. Na przykład, można ustawić nagłówki HTTP, send cookies, czy nawet przeprowadzić żądanie przez pośrednika.

Aby dowiedzieć się więcej o możliwościach pakietu `requests`, polecam przeczytać jego oficjalną dokumentację: [docs.python-requests.org](https://docs.python-requests.org).

## Zobacz także

* [Dokumentacja pakietu requests](https://docs.python-requests.org)
* [Tutorial "Pobieranie stron internetowych w Pythonie"](https://realpython.com/python-requests/) (w języku angielskim)