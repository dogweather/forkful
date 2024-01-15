---
title:                "Praca z json"
html_title:           "Fish Shell: Praca z json"
simple_title:         "Praca z json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest popularnym formatem danych wykorzystywanym w wielu dziedzinach, w tym w programowaniu. W tym artykule dowiesz się, jak używać Fish Shell do pracy z plikami JSON i jak wykorzystać to w codziennej pracy.

## Jak to zrobić

Fish Shell posiada wbudowane funkcje, które ułatwiają pracę z plikami JSON. W celu otworzenia i przetworzenia pliku możesz użyć polecenia "jq" (przełącznik "-R" umożliwia przechowywanie wyników w zmiennych):

```Fish Shell
jq -R '.' plik.json
```

Wykorzystując powyższy przykład, możesz wyświetlić całą zawartość pliku JSON w formacie, który jest łatwy do odczytania. Jeśli chcesz zmodyfikować konkretne dane w pliku, możesz użyć polecenia "jq" z opcją "-c" (pozwala na manipulowanie pojedynczymi wartościami):

```Fish Shell
jq -c '.klucz="nowa wartość"' plik.json
```

## Głębsza analiza

Polecenie "jq" jest niezwykle wszechstronnym narzędziem do manipulowania danymi w formacie JSON. Możesz wykorzystać go do filtrowania i przekształcania danych, a także do tworzenia nowych plików. Fish Shell zawiera również wiele innych przydatnych funkcji i poleceń do pracy z JSON, takich jak "walk" do nawigowania po strukturze pliku czy "merge" do łączenia kilku plików JSON w jeden.

## Zobacz również

- [Dokumentacja Fish Shell dotycząca pracy z JSON](https://fishshell.com/docs/current/cmds/jq.html)
- [Oficjalna strona formatu JSON](https://www.json.org/json-pl.html)
- [Poradnik dla początkujących: Jak pracować z plikami JSON w Fish Shell](https://dev.to/odpoczynski123/handling-json-files-with-fish-shell-1dp)