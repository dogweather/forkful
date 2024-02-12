---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:47:28.715522-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? | Co i Dlaczego?
Określenie długości stringa to po prostu zliczenie ilości znaków w nim zawartych. Programiści robią to, by walidować dane, manipulować tekstami czy po prostu określić ich zasoby.

## How to: | Jak to zrobić:
W Fish Shell mierzenie długości stringa jest banalnie proste.
```Fish Shell
set my_string "Dzień dobry"
echo $my_string | wc -m
```
Sample output | Przykładowy wynik:
```
12
```
```Fish Shell
set my_string "Informatyka"
string length $my_string
```
Sample output | Przykładowy wynik:
```
11
```
Tak, uwzględnia także spacje.

## Deep Dive | Głębsze zanurzenie:
Historia języków programowania pokazuje, że metody zmierzenia długości stringa ewoluowały. W starszych jezykach, jak C, należało samemu iterować przez string aż do napotkania znaku końca. Fish Shell uprościł to, dając wbudowaną komendę `string length`, która robi to za nas. Alternatywą jest użycie narzędzia `wc` z opcją `-m`, które zlicza znaki. W Fish, każda operacja na stringach uwzględnia także kodowanie UTF-8, więc długość jest liczona prawidłowo nawet jeśli string zawiera wielobajtowe znaki.

## See Also | Zobacz też:
- Dokumentacja Fish Shell `string` komendy: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Oficjalny tutorial Fish Shell: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- Porównanie narzędzia `wc`: [https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html#wc-invocation](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html#wc-invocation)
