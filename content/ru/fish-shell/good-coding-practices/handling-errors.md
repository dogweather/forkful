---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:09.548541-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u043F\u0435\u0440\u0435\u0445\u0432\
  \u0430\u0442\u044B\u0432\u0430\u0442\u044C \u043E\u0448\u0438\u0431\u043A\u0438\
  \ \u0432 Fish, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u0443 `status` \u0438 \u0443\u0441\u043B\u043E\
  \u0432\u0438\u044F. \u0414\u043E\u043F\u0443\u0441\u0442\u0438\u043C, `ping` \u043D\
  \u0435 \u0440\u0430\u0431\u043E\u0442\u0430\u0435\u0442; \u0432\u043E\u0442 \u043A\
  \u0430\u043A \u044D\u0442\u043E \u043E\u0431\u043D\u0430\u0440\u0443\u0436\u0438\
  \u0442\u044C."
lastmod: '2024-03-13T22:44:45.853868-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u043F\u0435\u0440\u0435\u0445\u0432\u0430\
  \u0442\u044B\u0432\u0430\u0442\u044C \u043E\u0448\u0438\u0431\u043A\u0438 \u0432\
  \ Fish, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 \u043A\
  \u043E\u043C\u0430\u043D\u0434\u0443 `status` \u0438 \u0443\u0441\u043B\u043E\u0432\
  \u0438\u044F."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как это сделать:
Чтобы перехватывать ошибки в Fish, используйте команду `status` и условия. Допустим, `ping` не работает; вот как это обнаружить:

```fish
ping -c 1 example.com
if not status is-success
    echo "С пингом что-то не так."
end
```

Пример вывода, если `ping` не выполняется:

```
С пингом что-то не так.
```

Чтобы обработать конкретный код ошибки, используйте `status --is`:

```fish
false
if status --is 1
    echo "Перехвачена ошибка с кодом 1."
end
```

Пример вывода:
```
Перехвачена ошибка с кодом 1.
```

Для более надежного подхода рассмотрите использование функции:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Пинг не удался со статусом $status"
        return 1
    end
end

try_ping
```

## Подробнее
Обработка ошибок в Fish не соответствует парадигме `try/catch`, которую вы можете знать из языков высокого уровня. Вместо этого у вас есть простые коды выхода, предоставляемые командой `status`.

Исторически в системах, подобных Unix, статус выхода `0` означает успех, в то время как любое ненулевое значение указывает на ошибку, что обычно отражает различные причины сбоя. Эту конвенцию используют большинство утилит командной строки и, следовательно, сам Fish.

Альтернативы проверкам статуса в Fish включают обработку сигналов через `trap` в других оболочках, но Fish предпочитает более явные проверки статуса, потому что это чище и менее подвержено побочным эффектам.

С точки зрения реализации, обработка ошибок в Fish остается простой, но мощной, во многом благодаря его не блокирующей природе и акценту на четком синтаксисе, как показано в примерах. Коды ошибок хорошо сочетаются с функциями, позволяя модульно и наглядно управлять ошибками.

## Смотрите также
- Документация Fish по условным операторам: https://fishshell.com/docs/current/language.html#conditionals
- Учебник Fish по обработке ошибок: https://fishshell.com/docs/current/tutorial.html#error-handling
