---
title:                "Запись в стандартный поток ошибок"
aliases:
- /ru/clojure/writing-to-standard-error.md
date:                  2024-01-29T00:06:21.087836-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Вывод в стандартный поток ошибок (`stderr`) - это способ вывода сообщений об ошибках и диагностической информации. Программисты используют его, чтобы отделить эти сообщения от обычного вывода (`stdout`), что упрощает отладку и логирование.

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
