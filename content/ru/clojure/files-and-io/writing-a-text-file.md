---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:40.947375-07:00
description: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432\u043A\
  \u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0441\u043E\
  \u0437\u0434\u0430\u043D\u0438\u0435 \u0438\u043B\u0438 \u0438\u0437\u043C\u0435\
  \u043D\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0445\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438 \u0441\u043E\u0445\u0440\u0430\u043D\
  \u0435\u043D\u0438\u0435 \u0438\u0445 \u0432 \u0444\u0430\u0439\u043B \u043D\u0430\
  \ \u0432\u0430\u0448\u0435\u043C \u043D\u043E\u0441\u0438\u0442\u0435\u043B\u0435\
  \ \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\
  \u0442\u2026"
lastmod: '2024-03-13T22:44:44.383974-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u0432\u043A\
  \u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0441\u043E\
  \u0437\u0434\u0430\u043D\u0438\u0435 \u0438\u043B\u0438 \u0438\u0437\u043C\u0435\
  \u043D\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0445\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438 \u0441\u043E\u0445\u0440\u0430\u043D\
  \u0435\u043D\u0438\u0435 \u0438\u0445 \u0432 \u0444\u0430\u0439\u043B \u043D\u0430\
  \ \u0432\u0430\u0448\u0435\u043C \u043D\u043E\u0441\u0438\u0442\u0435\u043B\u0435\
  \ \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\
  \u0442\u2026"
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
---

{{< edit_this_page >}}

## Что и Зачем?

Создание текстового файла включает в себя создание или изменение текстовых данных и сохранение их в файл на вашем носителе информации. Программисты делают это для ведения журналов данных, настроек конфигурации или экспорта отчетов в удобочитаемом формате.

## Как это сделать:

В Clojure вы используете функцию `spit` для записи данных в текстовый файл. Это просто:

```clojure
(spit "example.txt" "Привет, Мир! Это Clojure на связи.")
```

Функция `spit` берет имя файла и содержимое. Чтобы добавить содержимое, установите флаг `append`:

```clojure
(spit "example.txt" "\nДавайте добавим эту новую строку." :append true)
```

Пример вывода для `example.txt` после обеих операций:

```
Привет, Мир! Это Clojure на связи.
Давайте добавим эту новую строку.
```

## Глубокое Погружение

Функция `spit` в Clojure происходит из его библиотеки "Ввод/Вывод" - наследника легаси Lisp в части кратких операций с файлами. Альтернативы в Clojure включают `clojure.java.io/writer` для буферизированной записи и библиотеки вроде `slurp` для чтения файлов. При использовании `spit` помните, что она не предназначена для больших потоков данных из-за потенциальных проблем с памятью - используйте `writer` и цикл для обработки данных.

## См. также

- Документация Clojure для `spit`: [https://clojuredocs.org/clojure.core/spit](https://clojuredocs.org/clojure.core/spit)
- Оболочка Clojure для `java.io`: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
