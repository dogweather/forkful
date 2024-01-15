---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Bash: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie ciągu znaków na małe litery może być bardzo przydatne podczas pisania skryptów w Bashu. Pozwala to na ujednolicenie danych wejściowych oraz ułatwia wyszukiwanie i porównywanie tekstów.

## Jak to zrobić

```Bash
# Przykładowy skrypt konwertujący wszystkie litery na małe
#!/bin/bash

# Wczytanie ciągu znaków z klawiatury przez użytkownika
read -p "Wpisz ciąg znaków: " input

# Wykorzystanie komendy tr do zamiany wielkich liter na małe
echo "$input" | tr '[:upper:]' '[:lower:]'
```

**Przykładowe wyjście:**

```
> Wpisz ciąg znaków: PRZYkłADowY TEksT
przykładowy tekst
```

**Uwagi dotyczące kodu:**

- W powyższym przykładzie wykorzystano operator `|` do przekierowania danych z echo do komendy tr.
- Warto zauważyć, że komenda tr jest niezmienna, co oznacza, że ​​jeśli zostanie przekazany ciąg znaków zawierający już tylko małe litery, wynik zostanie zwrócony bez zmian.

## Dalej

Konwertowanie ciągu znaków na małe litery jest możliwe także przy użyciu innych poleceń, takich jak `awk` lub `sed`. Dodatkowo, można również wykorzystać wcięcia i pętle w celu stworzenia bardziej zaawansowanych skryptów konwertujących tekst.

## Zobacz także

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.pdf)
- [Poradnik po polsku dotyczący skryptów w Bashu](http://www.bash.bash.pl/)