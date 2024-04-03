---
date: 2024-01-20 17:47:28.715522-07:00
description: "How to: | Jak to zrobi\u0107: W Fish Shell mierzenie d\u0142ugo\u015B\
  ci stringa jest banalnie proste."
lastmod: '2024-03-13T22:44:35.829810-06:00'
model: gpt-4-1106-preview
summary: "W Fish Shell mierzenie d\u0142ugo\u015Bci stringa jest banalnie proste."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
