---
title:                "Fish Shell: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wydobycie podciągów jest często nieodłączną częścią programowania, szczególnie w językach programowania typu tekstowego. Jest to przydatna umiejętność, która pozwala na manipulowanie i przetwarzanie danych w bardziej precyzyjny sposób. W tym artykule pokażę Ci, jak wykorzystać funkcję wydobycia podciągów w Fish Shell.

## Jak To Zrobić

Aby wydobyć podciągi w Fish Shell, użyj funkcji ```string sub``` wraz z parametrami określającymi początek, koniec oraz długość wydobywanego podciągu. Przykładowy kod wyglądałby następująco:

```
string sub $string $start $end $length
```

Gdzie ```$string``` jest zmienną przechowującą oryginalny ciąg znaków, ```$start``` określa pozycję rozpoczęcia wydobycia, ```$end``` określa pozycję zakończenia wydobycia (opcjonalnie można wykorzystać parametr ```$length``` zamiast ```$end``` do określenia długości podciągu). 

Przykładowe użycie funkcji dla ciągu znaków "Kocham programowanie" wygląda następująco:
```
set string "Kocham programowanie"
string sub $string 0 6
```
Output:
```
Kocham
```

Możesz również wykorzystać funkcję ```string match``` do wydobycia podciągu za pomocą wyrażenia regularnego. Przykładowy kod wyglądałby następująco:
```
set string "To jest ciekawy artykuł"
string match -r "ciekawy (.*)" $string
```
Output:
```
ciekawy artykuł
```

## Również

Jeśli chcesz dowiedzieć się więcej o wydobywaniu podciągów w Fish Shell, możesz przejrzeć dokumentację na stronie Fish Shell lub zobaczyć inne przydatne przykłady tutaj:

- https://fishshell.com/docs/current/cmds/string-sub.html
- https://fishshell.com/docs/current/cmds/string-match.html

## Zobacz Również

Jeśli jesteś zainteresowany bardziej zaawansowanymi funkcjami obsługi ciągów znaków w Fish Shell, możesz również przeczytać mój wcześniejszy artykuł na temat formatowania znaków w tym języku.
https://example.com/polish/fish-shell-formatowanie-znakow