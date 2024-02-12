---
title:                "Запис до стандартної помилки"
date:                  2024-02-03T19:34:07.710996-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запис до стандартної помилки"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Запис у стандартну помилку (stderr) полягає у спрямовуванні повідомлень про помилки та діагностичних виводів у окремий канал, який відрізняється від стандартного виводу (stdout). Програмісти роблять це, щоб відрізнити звичайні результати програми від інформації про помилки, оптимізуючи процеси відлагодження та реєстрації.

## Як це зробити:
У Lua запис у stderr можна досягти за допомогою функції `io.stderr:write()`. Ось як ви можете написати просте повідомлення про помилку у стандартну помилку:

```lua
io.stderr:write("Помилка: Недійсний ввід.\n")
```

Якщо вам потрібно вивести змінну або об'єднати кілька фрагментів даних, конкатенуйте їх у функції запису:

```lua
local errorMessage = "Недійсний ввід."
io.stderr:write("Помилка: " .. errorMessage .. "\n")
```

**Приклад виводу на stderr:**
```
Помилка: Недійсний ввід.
```

Для більш складних сценаріїв або при роботі з більшими застосунками, ви можете розглядати сторонні бібліотеки логування, такі як LuaLogging. З використанням LuaLogging ви можете спрямовувати логи у різні місця, включаючи stderr. Ось короткий приклад:

Спочатку, переконайтеся, що LuaLogging встановлено за допомогою LuaRocks:

```
luarocks install lualogging
```

Потім, щоб написати повідомлення про помилку у stderr за допомогою LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Помилка: Недійсний ввід.")
```

Цей підхід пропонує перевагу стандартизованої реєстрації по всьому застосунку, з додатковою гнучкістю установки рівнів журналів (наприклад, ERROR, WARN, INFO) через простий API.