---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Fish Shell: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, przy pracując z tekstem, musimy usunąć wszystkie wystąpienia określonego wzorca znaków. Być może chcemy pozbyć się pewnych danych, poprawić błędy lub usunąć niepotrzebne formatowanie. Używając powłoki Fish, to zadanie jest szybkie i łatwe do wykonania.

## Jak to zrobić

Możemy skorzystać z polecenia `string replace` aby usunąć wszystkie wystąpienia określonego wzorca znaków z danego ciągu znaków. Przykładowe użycie wygląda tak:

```Fish Shell 
set hello "Hello, hello, hello"
echo $hello
# Output: Hello, hello, hello

string replace h "" $hello
echo $hello
# Output: ello, ello, ello
```

Polecenie `string replace` wymaga trzech argumentów: wzorca znaków do usunięcia, ciągu znaków w którym chcemy dokonać zamiany oraz ciągu znaków zastępczych (w naszym przypadku jest to pusty ciąg znaków). Możemy także użyć flagi `--count` aby określić ilość wystąpień wzorca do zastąpienia. Na przykład:

```Fish Shell
set numbers "1,2,3,4,5,6,7,8,9,10"
string replace "1," "ONE," --count=1 $numbers
echo $numbers
# Output: ONE,2,3,4,5,6,7,8,9,10
```

Powyższe przykłady pokazują jak usunąć pojedyncze wystąpienie wzorca znaków oraz określoną ilość wystąpień. Możemy także użyć polecenia `string match` aby określić, czy dany ciąg zawiera dany wzorzec. Przykładowe użycie wygląda tak:

```Fish Shell
set hello "Hello, world!"
if string match "*world*" $hello
    echo "Znaleziono szukany wzorzec"
else
    echo "Nie znaleziono szukanego wzorca"
end
# Output: Znaleziono szukany wzorzec
```

## Zagłębienie

W powyższych przykładach użyliśmy jasnych znaków rozpoczynających lub kończących wzorce. Możemy również używać symbolu gwiazdki `*` aby określić dowolne znaki. Na przykład:

```Fish Shell
set hello "Hello, world!"
if string match "H*llo*" $hello
    echo "Znaleziono szukany wzorzec"
else
    echo "Nie znaleziono szukanego wzorca"
end
# Output: Znaleziono szukany wzorzec
```

Symbol gwiazdki pozwala nam na znalezienie dowolnego ciągu znaków pomiędzy pierwszą i ostatnią literą wzorca. Możemy także użyć symbolu zapytania `?` aby określić pojedynczy znak. Na przykład:

```Fish Shell
set numbers "1,2,0,5,9"
string match ",?0,?" $numbers
# Output: 0
```

## Zobacz też

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current)
- [Zaawansowane operacje na ciągach znaków w Fish Shell](https://fishshell.com/docs/current/index.html#string_replacement)