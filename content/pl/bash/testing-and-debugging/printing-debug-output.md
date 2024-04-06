---
date: 2024-01-20 17:52:16.499802-07:00
description: "How to (Jak to zrobi\u0107) Wpisz powy\u017Cszy kod do skryptu i uruchom.\
  \ Zobaczysz warto\u015Bci zmiennych i przebieg p\u0119tli for."
lastmod: '2024-04-05T21:53:37.014520-06:00'
model: gpt-4-1106-preview
summary: "Zobaczysz warto\u015Bci zmiennych i przebieg p\u0119tli for."
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## How to (Jak to zrobić)
```Bash
#!/bin/bash

# Definiowanie zmiennej
moj_numer=123

# Prosty debug
echo "Debug: zmienna moj_numer wynosi $moj_numer"

# Warunkowy debug z użyciem zmiennej DEBUG
DEBUG=1
if [ "$DEBUG" -eq 1 ]; then
    echo "Warunkowy debug: moj_numer wynosi $moj_numer"
fi

# Użyj 'set -x' by śledzić wszystkie polecenia
set -x
inne_zmienne=("jabłko" "banan" "cytryna")
for owoc in "${inne_zmienne[@]}"; do
    echo "Owoc: $owoc"
done
set +x
```
Wpisz powyższy kod do skryptu i uruchom. Zobaczysz wartości zmiennych i przebieg pętli for.

## Deep Dive (Głębsze zanurzenie)
Historia Bash sięga UNIX-a i shella jego poprzednika. Nasz prosty `echo` to tylko czubek góry lodowej. Bash ma wbudowany `set -x` dla pełnych raportów debugowania. Ale, masz również inne narzędzia jak `trap` i `set -e` dla zaawansowanej kontroli błędów. Co ważniejsze, używaj narzędzi jak `logger` czy `Systemd Journal` w zastosowaniach produkcyjnych, by nie zanieczyszczać standardowego wyjścia.

## See Also (Zobacz również)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/) - Przewodnik po zaawansowanym skryptowaniu w Bash, w tym debugowaniu.
- [BashFAQ](http://mywiki.wooledge.org/BashFAQ) - Często zadawane pytania dotyczące Bash, z odpowiedziami ułatwiającymi zrozumienie jego zachowań.
