---
title:                "Wycinanie podłańcuchów"
aliases:
- /pl/bash/extracting-substrings.md
date:                  2024-01-20T17:45:17.492325-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyciąganie podciągów to wydobywanie konkretnych fragmentów tekstu z większych ciągów znaków. Programiści robią to, by manipulować danymi, weryfikować format, czy po prostu wydobyć potrzebne informacje.

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
