---
date: 2024-01-20 17:45:17.492325-07:00
description: "Jak to zrobi\u0107: Wycinanie podci\u0105g\xF3w w Bashu ma d\u0142ug\u0105\
  \ histori\u0119 \u2013 funkcjonalno\u015B\u0107 ta by\u0142a dost\u0119pna ju\u017C\
  \ w wersjach przed Bash 4. Pozwala to na obs\u0142ug\u0119 zar\xF3wno\u2026"
lastmod: '2024-04-05T21:53:36.999199-06:00'
model: gpt-4-1106-preview
summary: "Wycinanie podci\u0105g\xF3w w Bashu ma d\u0142ug\u0105 histori\u0119 \u2013\
  \ funkcjonalno\u015B\u0107 ta by\u0142a dost\u0119pna ju\u017C w wersjach przed\
  \ Bash 4."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## Jak to zrobić:
```Bash
# Wyodrębnienie podciągu używając indeksów
text="Bash jest super!"
echo ${text:5:4} # wypisze 'jest'

# Wyodrębnienie podciągu od końca ciągu znaków
echo ${text: -6:5} # wypisze 'super'

# Użycie sztuczki z 'expr substr' do wyciągania podciągów
podciag=$(expr substr "$text" 6 4)
echo $podciag # również wypisze 'jest'
```

## Głębsze zanurzenie
Wycinanie podciągów w Bashu ma długą historię – funkcjonalność ta była dostępna już w wersjach przed Bash 4. Pozwala to na obsługę zarówno prostych skryptów jak i złożonych skryptów do przetwarzania tekstu.

Alternatywnie, można użyć narzędzi zewnętrznych jak `awk`, `sed`, `cut`, jeśli wymagane jest coś bardziej skomplikowanego lub potrzebujemy większej kontroli nad procesem.

Szczegóły implementacji w Bashu to głównie operacje na zmiennych typu string, które wspierają indeksację i mogą być stosunkowo wolne w porównaniu z dedykowanymi programami do przetwarzania tekstu. Niemniej, w wielu przypadkach są wystarczające i znacznie prostsze w użyciu.

## Zobacz również:
- [Bash String Manipulation](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide: String Operations](http://tldp.org/LDP/abs/html/string-manipulation.html)
- Stack Overflow odpowiedzi dotyczące [manipulacji ciągami znaków w Bashu](https://stackoverflow.com/questions/tagged/string+bash)
