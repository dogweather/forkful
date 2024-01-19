---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego? 

Konwersja ciągu na małe litery to proces zamiany wszystkich liter wielkich na małe w danym ciągu. Programiści robią to, aby porównywać ciągi tekstu niezależnie od wielkości liter.

## Jak to zrobić:

Poniżej znajduje się przykładowy kod Fish Shell, który konwertuje napis na małe litery:

```Fish Shell
function małe_litery -a string
    echo (string lower $string)
end

set wielkie "WIELKIE LITERY"
małe_litery $wielkie
```

Po wykonaniu powyższego skryptu, wynikiem powinny być wszystkie małe litery, jak pokazano poniżej:

```Fish Shell
> wielkie litery
```

## Deep Dive:

W kontekście historycznym, różne języki programowania implementują tę funkcję na różne sposoby. Na przykład, w Pythonie używamy metody `.lower()`, a w JavaScript - metody `.toLowerCase()`. 

Alternatywnie, niektóre języki, takie jak C, wymagają ręcznego przepisania ciągu i konwersji każdej litery osobno. Wybór metody zależy od języka, który używasz, i od specyfiki zadania.

W Fish Shell, 'string lower' jest wbudowaną funkcją, która nie tylko konwertuje ciąg na małe litery, ale także obsługuje różne zestawy znaków, takie jak UTF-8.

## Zobacz również:

1. Oficjalna dokumentacja Fish Shell - String: https://fishshell.com/docs/current/cmds/string.html
2. Stack Overflow dla Fish Shell: https://stackoverflow.com/questions/tagged/fish