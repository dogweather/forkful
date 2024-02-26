---
date: 2024-01-20 17:47:00.288176-07:00
description: "Znalezienie d\u0142ugo\u015Bci \u0142a\u0144cucha polega na ustaleniu,\
  \ ile znak\xF3w zawiera dany tekst. Programi\u015Bci robi\u0105 to, by zarz\u0105\
  dza\u0107 danymi tekstowymi \u2013 sprawdza\u0107\u2026"
lastmod: '2024-02-25T18:49:33.939576-07:00'
model: gpt-4-1106-preview
summary: "Znalezienie d\u0142ugo\u015Bci \u0142a\u0144cucha polega na ustaleniu, ile\
  \ znak\xF3w zawiera dany tekst. Programi\u015Bci robi\u0105 to, by zarz\u0105dza\u0107\
  \ danymi tekstowymi \u2013 sprawdza\u0107\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
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
