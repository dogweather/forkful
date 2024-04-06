---
date: 2024-01-20 17:43:14.771222-07:00
description: "Jak to zrobi\u0107: Pobieranie stron ma bogat\u0105 histori\u0119, wystartowa\u0142\
  o wraz z pojawieniem si\u0119 przegl\u0105darek. Alternatywami dla `curl` i `wget`\
  \ s\u0105 narz\u0119dzia\u2026"
lastmod: '2024-04-05T21:53:37.011181-06:00'
model: gpt-4-1106-preview
summary: "Pobieranie stron ma bogat\u0105 histori\u0119, wystartowa\u0142o wraz z\
  \ pojawieniem si\u0119 przegl\u0105darek."
title: Pobieranie strony internetowej
weight: 42
---

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
