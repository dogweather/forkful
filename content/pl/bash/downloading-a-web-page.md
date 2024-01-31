---
title:                "Pobieranie strony internetowej"
date:                  2024-01-20T17:43:14.771222-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Pobieranie stron internetowych to proces zapisywania zawartości strony na swoim komputerze. Programiści robią to, by analizować strukturę strony, scraować dane lub testować aplikacje.

## Jak to zrobić:
```
# Pobranie strony i zapisanie do pliku przy użyciu cURL
curl http://example.com -o example_page.html

# Wyświetlenie wyników
cat example_page.html
```

```
# Pobieranie strony z wykorzystaniem wget
wget http://example.com

# Wynik będzie zapisany jako 'index.html' w bieżącym katalogu
ls
```

## Deep Dive
Pobieranie stron ma bogatą historię, wystartowało wraz z pojawieniem się przeglądarek. Alternatywami dla `curl` i `wget` są narzędzia graficzne i biblioteki programistyczne jak `httrack` czy `BeautifulSoup` (Python). `curl` jest świetny do szybkich operacji w terminalu, natomiast `wget` radzi sobie lepiej z pobieraniem w trybie rekursywnym i obsługą dużych pobierań.

## Zobacz także:
- [cURL man page](https://curl.haxx.se/docs/manpage.html)
- [Wget manual](https://www.gnu.org/software/wget/manual/wget.html)
- [Introduction to Web Scraping using Python](https://realpython.com/python-web-scraping-practical-introduction/)
