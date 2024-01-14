---
title:                "Fish Shell: Zmiana na wielkie litery ciągu znaków"
simple_title:         "Zmiana na wielkie litery ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, z pewnością spotkałeś się z sytuacją, w której musiałeś zmienić wielkość liter w tekście. Możliwe, że korzystałeś z języka programowania, który miał wbudowaną funkcję do tej operacji. Jednak, jeśli używasz powłoki Fish Shell, musisz samodzielnie napisać ten kod. W tym artykule dowiesz się, dlaczego warto to zrobić i jak to zrobić.

## Jak to zrobić

Fish Shell oferuje proste i wygodne rozwiązanie, aby zmienić wielkość liter w dowolnym ciągu znaków. Wystarczy zastosować wbudowaną funkcję `string capitalize`, która przyjmuje w parametrze ciąg znaków, a zwraca wynik z pierwszą literą każdego słowa zapisanego wielkimi literami. Przykładowy kod i wynik w konsoli może wyglądać następująco:

```Fish Shell
# Przykładowy tekst
set text "to jest tekst przykładowy"

# Użycie funkcji capitalize
string capitalize $text

# Wynik
To Jest Tekst Przykładowy
```

W powyższym przykładzie zastosowaliśmy funkcję `string capitalize` do zmiany wielkości liter w tekście z małych na duże. Możemy również wykorzystać funkcję `string uppercase` lub `string lowercase` dla bardziej szczegółowej kontroli nad zmianą wielkości liter.

## Deep Dive

Jeśli jesteś ciekaw, jak dokładnie działa funkcja `string capitalize`, warto prześledzić kod źródłowy tej funkcji. Dzięki temu będziesz miał lepsze zrozumienie procesu zmiany wielkości liter w Fish Shell. Kod możesz znaleźć w dokumentacji na oficjalnej stronie Fish Shell lub w repozytorium na GitHubie.

## Zobacz również

* Dokumentacja Fish Shell: https://fishshell.com/docs/current/index.html
* Repozytorium Fish Shell na GitHubie: https://github.com/fish-shell/fish-shell