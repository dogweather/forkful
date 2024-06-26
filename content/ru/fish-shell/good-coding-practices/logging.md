---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:51.769511-07:00
description: "\u041A\u0430\u043A: \u0412 Fish \u043B\u043E\u0433\u0438\u0440\u043E\
  \u0432\u0430\u043D\u0438\u0435 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\
  \u044C \u0442\u0430\u043A\u0438\u043C \u0436\u0435 \u043F\u0440\u043E\u0441\u0442\
  \u044B\u043C, \u043A\u0430\u043A \u043F\u0435\u0440\u0435\u043D\u0430\u043F\u0440\
  \u0430\u0432\u043B\u0435\u043D\u0438\u0435 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442\u043D\u044B\u0445 \u043F\u043E\u0442\u043E\u043A\u043E\u0432 \u0432\u044B\
  \u0432\u043E\u0434\u0430 \u0438 \u043E\u0448\u0438\u0431\u043E\u043A \u0432 \u0444\
  \u0430\u0439\u043B. \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0441\u0434\u0435\
  \u043B\u0430\u0435\u043C \u0437\u0430\u043F\u0438\u0441\u044C \u0432 \u043B\u043E\
  \u0433 \u043E\u2026"
lastmod: '2024-03-13T22:44:45.852093-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Fish \u043B\u043E\u0433\u0438\u0440\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u0442\u0430\u043A\
  \u0438\u043C \u0436\u0435 \u043F\u0440\u043E\u0441\u0442\u044B\u043C, \u043A\u0430\
  \u043A \u043F\u0435\u0440\u0435\u043D\u0430\u043F\u0440\u0430\u0432\u043B\u0435\u043D\
  \u0438\u0435 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u044B\u0445\
  \ \u043F\u043E\u0442\u043E\u043A\u043E\u0432 \u0432\u044B\u0432\u043E\u0434\u0430\
  \ \u0438 \u043E\u0448\u0438\u0431\u043E\u043A \u0432 \u0444\u0430\u0439\u043B."
title: "\u0416\u0443\u0440\u043D\u0430\u043B\u0438\u0440\u043E\u0432\u0430\u043D\u0438\
  \u0435"
weight: 17
---

## Как:
В Fish логирование может быть таким же простым, как перенаправление стандартных потоков вывода и ошибок в файл. Давайте сделаем запись в лог о времени начала и окончания работы нашего скрипта.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Скрипт запущен" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Скрипт завершён" >> my_app.log
end

log_start
# ... задачи вашего скрипта ...
log_end

cat my_app.log
```

Вот что вы увидите в `my_app.log`:

```
2023-04-01 10:35:47  - Скрипт запущен
2023-04-01 10:36:02  - Скрипт завершён
```

Для более сложного логирования вы можете использовать функции с параметрами для уровня логирования и сообщений:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Это информационное сообщение."
log_message ERROR "Что-то пошло не так!"
```

Пример вывода в `my_app.log` будет:
```
2023-04-01 10:35:47 [INFO] Это информационное сообщение.
2023-04-01 10:35:49 [ERROR] Что-то пошло не так!
```

## Глубже
Исторически логирование в shell-скриптах выполнялось с помощью множества команд `echo`, и хотя это определённо по-прежнему вариант, реализация более сложных систем может быть вызовом. У Fish нет встроенного механизма логирования, как в некоторых других оболочках или языках программирования, поэтому часто приходится создавать своё.

Альтернативы встроенной команде `echo` в Fish для логирования включают в себя Unix-инструменты, такие как `syslog` или `logger`, которые взаимодействуют с системным демоном логирования и обеспечивают более интегрированный подход к логированию системных событий.

Простота Fish позволяет создавать функции для управления подробностью логирования, устанавливая разные уровни, которые можно включать или выключать. Некоторые реализации могут даже включать название скрипта, номер строки и временную метку, что упрощает процесс отслеживания шагов, приведших к событию.

## Смотрите также
- Документация по оболочке Fish о написании функций: https://fishshell.com/docs/current/#syntax-function
- Основные советы по написанию скриптов для Shell: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Руководство по протоколу Syslog: https://tools.ietf.org/html/rfc5424
