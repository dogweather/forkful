---
date: 2024-01-20 17:41:47.344450-07:00
description: "How to: (Jak to zrobi\u0107?) Bash wykorzystuje wyra\u017Cenia regularne\
  \ (regex), pot\u0119\u017Cne narz\u0119dzie, kt\xF3re zapocz\u0105tkowano ju\u017C\
  \ w latach 50-tych. Za pomoc\u0105 regex\u2026"
lastmod: '2024-04-05T22:50:49.890392-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) Bash wykorzystuje wyra\u017Cenia regularne (regex),\
  \ pot\u0119\u017Cne narz\u0119dzie, kt\xF3re zapocz\u0105tkowano ju\u017C w latach\
  \ 50-tych."
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to: (Jak to zrobić?)
```Bash
# Przykład 1: Usuń wszystkie wystąpienia litery 'a'
tekst="banana"
echo "${tekst//a/}"

# Wynik: bnn

# Przykład 2: Usuń cyfry z ciągu znaków
tekst="abc123"
echo "${tekst//[0-9]/}"

# Wynik: abc

# Przykład 3: Usuń wszystkie znaki oprócz cyfr
tekst="abc123"
echo "${tekst//[^0-9]/}"

# Wynik: 123
```

## Deep Dive (Wnikliwa analiza)
Bash wykorzystuje wyrażenia regularne (regex), potężne narzędzie, które zapoczątkowano już w latach 50-tych. Za pomocą regex można z łatwością wyszukać, zamienić lub usunąć konkretne znaki czy wzorce.

Alternatywami dla wbudowanych funkcji Bash są zewnętrzne programy jak `sed` czy `awk`, które oferują jeszcze więcej opcji manipulowania tekstem.

Szczegółowość implementacji zależy od potrzeb. W Bashu można operować na zmiennych tekstowych bezpośrednio w skrypcie, a `//` oznacza usunięcie wszystkich wystąpień wzorca, podczas gdy `/` usunie tylko pierwsze jego wystąpienie.

## See Also (Zobacz także)
- `man bash` - manual do Bash, sekcja o wyrażeniach regularnych i pattern matching.
- `man sed` - manual do sed, edytor strumieniowy do manipulacji tekstem.
- `man awk` - manual do awk, język programowania przeznaczony do przetwarzania i analizowania danych tekstowych.

Dodatkowo poczytać można na temat wyrażeń regularnych, aby lepiej zrozumieć, jak tworzyć bardziej złożone wzorce:
- [RegExr](https://regexr.com/) - strona do nauki i testowania wyrażeń regularnych.
- [GNU Bash manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html) - online manual do Bash zawierający informacje o pattern matching.
