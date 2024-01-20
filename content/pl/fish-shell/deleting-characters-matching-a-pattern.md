---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usunięcie znaków zgodnych z wzorcem to metoda pozbycia się określonego typu danych z ciągu. Programiści robią to, aby uporządkować dane, na przykład kiedy chcą usunąć niepotrzebne przecinki lub spacje.

## Jak to zrobić?
Podstawowym sposobem usunięcia znaków zgodnych z wzorcem w Fish Shell jest użycie komendy `string replace`. Na przykład, jeśli chcemy usunąć wszystkie wystąpienia znaku '#' z napisu, użylibyśmy poniższego kodu:

```fish
set string '#To jest przykładowy # łańcuch.'
set new_string (string replace -r -a '#' '' -- $string)
echo $new_string
```
To polecenie zmienia pierwotny ciąg, zamieniając wszystkie znaki '#' na pusty ciąg, co skutkuje usunięciem ich. Wyjście z tego kodu będzie wyglądało tak:

```fish
To jest przykładowy  łańcuch.
```

## Głębsze spojrzenie
Pożądanie usunięcia znaków zgodnych z wzorcem nie jest niczym nowym w programowaniu. Wiele starszych języków programowania, takich jak sed i awk, już to miało. Fish Shell nabiera na popularności dzięki łatwości z jaką pozwala na wykonywanie tego typu operacji. Alternatywą dla metody `string replace` jest użycie komend `tr` or `sed` w połączeniu z `pipe`.

Pamiętaj, że "string replace" w Fish Shell został zaimplementowany w taki sposób, aby elastycznie obsługiwać dopasowanie wzorców. Możesz użyć wyrażeń regularnych do dopasowania bardziej złożonych wzorców i usuwania ich.

## Zobacz również
Polecamy przejrzenie oficjalnej dokumentacji Fish Shell do zapoznania się z innymi metodami manipulacji ciągami:

- Dokumentacja Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Wprowadzenie do języka Fish: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- Rosetta Code: Przykłady kodu Fish Shell: [https://rosettacode.org/wiki/Category:Fish](https://rosettacode.org/wiki/Category:Fish)