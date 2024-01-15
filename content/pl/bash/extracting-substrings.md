---
title:                "Wycinanie podłańcuchów"
html_title:           "Bash: Wycinanie podłańcuchów"
simple_title:         "Wycinanie podłańcuchów"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Znajdywanie i wyodrębnianie podciągów tekstowych jest często niezbędnym krokiem w wielu programach Bash. Nie tylko ułatwia to manipulację i analizę danych, ale także może przyspieszyć wykonanie skryptu poprzez zmniejszenie potrzebnej ilości danych do przetworzenia.

## Jak to zrobić

W Bash istnieje kilka sposobów na ekstrakcję podciągów z tekstu, w tym wykorzystanie `awk`, `cut` oraz operatorów `#` i `%`. Przedstawione są poniżej podstawowe przykłady kodu oraz wyników.

### Wykorzystanie `awk`

```Bash
fruit="jabłko gruszka pomarańcza"
echo $fruit | awk '{print $2}' # wypisze "gruszka"
echo $fruit | awk '{print substr($1, 1, 4)}' # wypisze "jabł"
```

### Wykorzystanie `cut`

```Bash
fruit="jabłko-gruszka-pomarańcza"
echo $fruit | cut -d "-" -f 2 # wypisze "gruszka"
```

### Wykorzystanie operatorów # i %

```Bash
fruit="jabłko-gruszka-pomarańcza"
echo ${fruit#*-} # wypisze "gruszka-pomarańcza"
echo ${fruit%-*} # wypisze "jabłko-gruszka"
```

## Deep Dive

Funkcja `substr` w Bash pozwala na wyodrębnienie podciągu z danego tekstu na podstawie jego indeksów. Pierwszy argument określa, od którego indeksu ma rozpocząć się ekstrakcja, a drugi określa długość wyodrębnionego podciągu. Można również wykorzystać `length` w drugim argumencie, aby wyciągnąć podciąg od określonego indeksu do końca tekstu. Natomiast wykorzystanie `cut` pozwala na wycięcie określonej części tekstu na podstawie ustalonego separatora. Ostatni przykład wykorzystuje operator `#` lub `%` do usunięcia określonego tekstu z lewej lub prawej strony.

## Zobacz również

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Podstawowe operacje tekstowe w Bash](https://www.programiz.com/bash-scripting/text-processing)
- [Wyjaśnienie podciągów w Bash](https://www.baeldung.com/linux/substring-extraction-linux)