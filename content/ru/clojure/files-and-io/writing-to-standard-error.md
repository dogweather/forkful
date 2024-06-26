---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:21.087836-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430 \u0432 \u0441\
  \u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\
  \u043A \u043E\u0448\u0438\u0431\u043E\u043A \u0432 Clojure \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u044E\u0442 `binding` \u0441 `*err*`. \u0412\u043E\u0442\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440\
  ."
lastmod: '2024-03-13T22:44:44.380329-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430 \u0432 \u0441\u0442\
  \u0430\u043D\u0434\u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A\
  \ \u043E\u0448\u0438\u0431\u043E\u043A \u0432 Clojure \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u044E\u0442 `binding` \u0441 `*err*`."
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
weight: 25
---

## Как это сделать:
Для вывода в стандартный поток ошибок в Clojure используют `binding` с `*err*`. Вот простой пример:

```Clojure
(binding [*err* *out*]
  (println "Это пойдет в стандартный поток ошибок"))
```

Пример вывода (в вашем терминале):

```
$ clj your_script.clj 2> error.log
$ cat error.log
Это пойдет в стандартный поток ошибок
```

Этот фрагмент кода связывает `*err*` с `*out*`, который является стандартным выводом, так что вы можете видеть, что обычно пойдет в `stderr`.

## Глубокое Погружение
Исторически в системах Unix было два отдельных потока вывода, `stdout` и `stderr`, для разных типов данных. В Clojure `*out*` относится к `stdout`, а `*err*` к `stderr`. Альтернативы использованию `binding` включают прямое использование Java interop (например, `(.println System/err "сообщение")`). С точки зрения реализации, `*err*` является динамической переменной, позволяющей использовать привязки локальные для потока — нюанс, который может влиять на то, как ошибки логируются в приложениях с конкурентностью.

## Смотрите Также
- Документация Clojure по `*err*`: https://clojuredocs.org/clojure.core/*err*
- Документация Clojure по `binding`: https://clojuredocs.org/clojure.core/binding
- Java API для `PrintStream` (который является `System/err`): https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html

Для более широкого понимания стандартных потоков также могут быть полезны следующие источники:
- Википедия о Стандартных Потоках: https://en.wikipedia.org/wiki/Standard_streams
- Документация по стандартным потокам Unix: `man stdio`
