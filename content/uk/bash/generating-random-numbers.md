---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:48:25.942896-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Згенерувати випадкове число у Bash – це як рулетка, але командний рядок вирішує. Програмісти це роблять для тестів, симуляцій чи коли хочуть щось трохи непередбачуване.

## How to: (Як зробити:)
```Bash
# Generate a random number between 1 and 100
echo $((RANDOM % 100 + 1))
```
Output might look like:
```
57
```

```Bash
# Seed RANDOM for scripts
RANDOM=$(date +%s)
echo $((RANDOM % 100 + 1))
```
Sample output could be:
```
42
```

## Deep Dive (Поглиблений підхід)
'RANDOM' у Bash не новинка; використовують з часів Bash 2.0 (1996 рік). Значення 'RANDOM' – це псевдовипадкове, значить, його можна "посіяти" командою 'RANDOM=seed'. У якості альтернативи є 'shuf', 'awk' і, ззовні, 'openssl' для криптографічно безпечних чисел. 'RANDOM' зручний, але не найкращий для критично важливих випадковостей.

## See Also (Дивіться також)
- `man bash` (для деталей про вбудовані змінні, включаючи RANDOM).
- GNU Coreutils: `info coreutils 'shuf invocation'` (для команди shuf).
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/randomvar.html