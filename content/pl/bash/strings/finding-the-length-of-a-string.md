---
date: 2024-01-20 17:47:00.288176-07:00
description: "Jak to zrobi\u0107: W przesz\u0142o\u015Bci, rozwi\u0105zania typu `expr`\
  \ by\u0142y bardziej powszechne, lecz ${#string} oferuje wi\u0119ksz\u0105 wydajno\u015B\
  \u0107 i czytelno\u015B\u0107, staj\u0105c si\u0119\u2026"
lastmod: '2024-04-05T21:53:37.001506-06:00'
model: gpt-4-1106-preview
summary: "W przesz\u0142o\u015Bci, rozwi\u0105zania typu `expr` by\u0142y bardziej\
  \ powszechne, lecz ${#string} oferuje wi\u0119ksz\u0105 wydajno\u015B\u0107 i czytelno\u015B\
  \u0107, staj\u0105c si\u0119 standardem w nowszych skryptach."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

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
