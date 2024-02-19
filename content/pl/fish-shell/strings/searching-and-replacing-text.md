---
aliases:
- /pl/fish-shell/searching-and-replacing-text/
date: 2024-01-20 17:58:00.452548-07:00
description: "Wyszukiwanie i zamiana tekstu to zmiana jednego ci\u0105gu znak\xF3\
  w na inny. Programi\u015Bci robi\u0105 to, aby szybko poprawia\u0107 b\u0142\u0119\
  dy, aktualizowa\u0107 kod lub zmienia\u0107 dane."
lastmod: 2024-02-18 23:08:50.020613
model: gpt-4-1106-preview
summary: "Wyszukiwanie i zamiana tekstu to zmiana jednego ci\u0105gu znak\xF3w na\
  \ inny. Programi\u015Bci robi\u0105 to, aby szybko poprawia\u0107 b\u0142\u0119\
  dy, aktualizowa\u0107 kod lub zmienia\u0107 dane."
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## What & Why? - Co i dlaczego?
Wyszukiwanie i zamiana tekstu to zmiana jednego ciągu znaków na inny. Programiści robią to, aby szybko poprawiać błędy, aktualizować kod lub zmieniać dane.

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
