---
title:                "Drukowanie komunikatów debugowania"
aliases:
- /pl/bash/printing-debug-output.md
date:                  2024-01-20T17:52:16.499802-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Debugowanie to sztuka wykrywania błędów. Szybko pokazujemy zmienne i komunikaty, by zrozumieć, co się dzieje w skrypcie. Dlaczego? Bo informatyk bez debugowania to jak lekarz bez stetoskopu – leci na ślepo.

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
