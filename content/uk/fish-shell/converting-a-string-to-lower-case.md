---
title:                "Перетворення рядка у нижній регістр"
aliases:
- uk/fish-shell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:47.522266-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Переведення рядка у нижній регістр— це процес зміни всіх літер у рядку на малі. Програмісти роблять це для уніфікації текстових даних, полегшення пошуку та порівняння рядків.

## How to: (Як зробити:)
В Fish Shell перетворення рядка у нижній регістр робиться легко. Використайте вбудовану команду `string`.

```fish
# Перетворення звичайного рядка
echo 'Hello, World!' | string lower
# Вивід: hello, world!

# Застосування до змінної
set phrase 'Fish SHELL Is Fun'
echo $phrase | string lower
# Вивід: fish shell is fun
```

## Deep Dive (Поглиблений аналіз)
Fish Shell — це сучасний командний інтерпретатор, що включає в себе зручні функції для маніпуляції текстом. Команда `string lower` додана для полегшення обробки рядків. Замість використання зовнішніх програм, як `awk` або `tr`, Fish дозволяє робити це "з коробки".

В історичному контексті, багато оболонок (shell) спиралися на зовнішні утиліти для обробки тексту, що могло ускладнити сценарії та знизити продуктивність. Fish Shell зайшов далі, надаючи поєднання продуктивності та зручності.

Альтернативою є створення скриптів у мовах вищого рівня, як Python або Perl, що мають багаті можливості по роботі з текстом, але це може бути надмірним для простих завдань.

В Fish, перетворення рядка у нижній регістр - це чисто вбудована операція, яка використовує механізми Юнікоду для правильної обробки символів в різних мовах, що особливо актуально в багатомовних додатках та середовищах.

## See Also (Дивіться також)
- [Fish Documentation on String](https://fishshell.com/docs/current/cmds/string.html)
- [Unicode Standard](https://www.unicode.org/standard/standard.html)
- [POSIX Shell Command Language](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html)
