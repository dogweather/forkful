---
title:                "Znalezienie długości ciągu znaków"
aliases:
- /pl/bash/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:00.288176-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Znalezienie długości łańcucha polega na ustaleniu, ile znaków zawiera dany tekst. Programiści robią to, by zarządzać danymi tekstowymi – sprawdzać poprawność inputu, porównywać wartości czy manipulować ciągami.

## Jak to zrobić:
```Bash
# Użycie wbudowanego wyrażenia ${#string}
tekst="Witaj, świecie!"
echo "Długość tekstu: ${#tekst}"    # Wyświetla: Długość tekstu: 15

# Alternatywna metoda przy użyciu 'expr'
echo "Długość tekstu: $(expr length "$tekst")"    # Wyświetla: Długość tekstu: 15
```

## Dogłębny wgląd:
W przeszłości, rozwiązania typu `expr` były bardziej powszechne, lecz ${#string} oferuje większą wydajność i czytelność, stając się standardem w nowszych skryptach. Opcje takie jak `awk` czy `wc -m` nadal istnieją, ale są rzadziej używane do tej konkretnej operacji.

Implementacja zależy od kolejności bajtów w pamięci i lokalizacji systemu. Ważne, gdy pracujemy z wielobajtowymi zestawami znaków, jak UTF-8 – tu pojedynczy znak może być reprezentowany przez więcej niż jeden bajt.

## Zobacz również:
- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide: String Operations](https://tldp.org/LDP/abs/html/string-manipulation.html)
- Oficjalna dokumentacja GNU Bash: https://www.gnu.org/software/bash/
- [Bash FAQ – How do I determine the length of a string?](http://mywiki.wooledge.org/BashFAQ/071)
