---
date: 2024-01-26 00:54:09.316606-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0429\
  \u043E\u0431 \u043F\u0435\u0440\u0435\u0445\u043E\u043F\u0438\u0442\u0438 \u043F\
  \u043E\u043C\u0438\u043B\u043A\u0438 \u0432 Fish, \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0439\u0442\u0435 \u043A\u043E\u043C\u0430\u043D\
  \u0434\u0443 `status` \u0442\u0430 \u0443\u043C\u043E\u0432\u043D\u0456 \u043E\u043F\
  \u0435\u0440\u0430\u0442\u043E\u0440\u0438. \u0421\u043A\u0430\u0436\u0456\u043C\
  \u043E, `ping` \u043D\u0435 \u0441\u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0432\
  ; \u043E\u0441\u044C \u044F\u043A \u0446\u0435 \u0432\u0438\u044F\u0432\u0438\u0442\
  \u0438."
lastmod: '2024-03-13T22:44:50.081598-06:00'
model: gpt-4-1106-preview
summary: "\u0429\u043E\u0431 \u043F\u0435\u0440\u0435\u0445\u043E\u043F\u0438\u0442\
  \u0438 \u043F\u043E\u043C\u0438\u043B\u043A\u0438 \u0432 Fish, \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0439\u0442\u0435 \u043A\u043E\u043C\
  \u0430\u043D\u0434\u0443 `status` \u0442\u0430 \u0443\u043C\u043E\u0432\u043D\u0456\
  \ \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440\u0438."
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
weight: 16
---

## Як це робити:
Щоб перехопити помилки в Fish, використовуйте команду `status` та умовні оператори. Скажімо, `ping` не спрацював; ось як це виявити:

```fish
ping -c 1 example.com
if not status is-success
    echo "Щось рибне трапилось з ping."
end
```

Приклад виводу, якщо `ping` не вдасться:

```
Щось рибне трапилось з ping.
```

Для обробки конкретного коду помилки, використовуйте `status --is`:

```fish
false
if status --is 1
    echo "Вловлено помилку з кодом 1."
end
```

Приклад виводу:
```
Вловлено помилку з кодом 1.
```

Для більш надійного підходу, розгляньте використання функції:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping не вдався зі статусом $status"
        return 1
    end
end

try_ping
```

## Поглиблене вивчення
Обробка помилок у Fish не відповідає парадигмі `try/catch`, яку ви могли б знати з вищих мов програмування. Натомість, у вас є прості статуси виходу, які надає команда `status`.

Історично, в системах, подібних до Unix, статус виходу `0` означає успіх, тоді як будь-яке ненульове значення свідчить про помилку, яка зазвичай відображає різні причини невдачі. Ця конвенція використовується більшістю утиліт командного рядка і, отже, самим Fish.

Альтернативи перевіркам `status` у Fish включають обробку сигналів через `trap` в інших оболонках, але Fish віддає перевагу більш явній перевірці статусу, оскільки це чистіше і менш схильне до побічних ефектів.

З точки зору реалізації, обробка помилок у Fish залишається простою, але потужною, завдяки її неблокуючій природі і акценту на чіткому синтаксисі, як показано в прикладах. Коди помилок гарно інтегруються з функціями, дозволяючи створювати модульне та зрозуміле управління помилками.

## Дивись також
- Документація Fish про умовні оператори: https://fishshell.com/docs/current/language.html#conditionals
- Посібник Fish про обробку помилок: https://fishshell.com/docs/current/tutorial.html#error-handling
