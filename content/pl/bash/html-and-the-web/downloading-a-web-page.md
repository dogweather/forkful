---
date: 2024-01-20 17:43:14.771222-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.583786-06:00'
model: gpt-4-1106-preview
summary: .
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
