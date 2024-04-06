---
date: 2024-01-20 17:58:00.452548-07:00
description: "How to: - Jak to zrobi\u0107: Wyszukiwanie i zamiana tekstu ma swoje\
  \ korzenie w edycji tekstu i przetwarzaniu komputerowym z lat 60. Wcze\u015Bniejsze\
  \ narz\u0119dzia jak\u2026"
lastmod: '2024-04-05T22:50:50.167243-06:00'
model: gpt-4-1106-preview
summary: "- Jak to zrobi\u0107: Wyszukiwanie i zamiana tekstu ma swoje korzenie w\
  \ edycji tekstu i przetwarzaniu komputerowym z lat 60. Wcze\u015Bniejsze narz\u0119\
  dzia jak ed czy ex wp\u0142yn\u0119\u0142y na powstanie `sed`, kt\xF3ry jest standardem\
  \ w Unixach od 1974 roku. Alternatyw\u0105 dla `sed` jest `awk`, kt\xF3re r\xF3\
  wnie\u017C oferuje zaawansowane operacje na tek\u015Bcie. Fish shell nie ma w\u0142\
  asnego wewn\u0119trznego narz\u0119dzia do tego zadania, dlatego korzysta si\u0119\
  \ z zewn\u0119trznych jak `sed` czy `awk`. W implementacji Fish wystarczy u\u017C\
  y\u0107 potoku (pipe) do przekazania tekstu do tych narz\u0119dzi."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## How to: - Jak to zrobić:
```Fish Shell
# Wyszukaj 'stary_tekst' i zamień go na 'nowy_tekst' w pliku 'plik.txt'
sed 's/stary_tekst/nowy_tekst/g' plik.txt

# Aby zapisać zmiany w pliku, możesz użyć flagi -i
sed -i 's/stary_tekst/nowy_tekst/g' plik.txt

# Przykład użycia wyrażeń regularnych do wyszukiwania cyfr i zamiany na 'liczba'
echo "To jest rok 2023" | sed 's/[0-9]+/liczba/g'
```
Output:
```
To jest rok liczba
```

## Deep Dive - W głębi tematu:
Wyszukiwanie i zamiana tekstu ma swoje korzenie w edycji tekstu i przetwarzaniu komputerowym z lat 60. Wcześniejsze narzędzia jak ed czy ex wpłynęły na powstanie `sed`, który jest standardem w Unixach od 1974 roku. Alternatywą dla `sed` jest `awk`, które również oferuje zaawansowane operacje na tekście. Fish shell nie ma własnego wewnętrznego narzędzia do tego zadania, dlatego korzysta się z zewnętrznych jak `sed` czy `awk`. W implementacji Fish wystarczy użyć potoku (pipe) do przekazania tekstu do tych narzędzi.

## See Also - Zobacz również:
- Dokumentacja `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Dokumentacja Fish Shell dotycząca potoków: https://fishshell.com/docs/current/index.html#syntax-pipe
- Wprowadzenie do wyrażeń regularnych: https://www.regular-expressions.info/
