---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:08.787484-07:00
description: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438\u0437 \u0444\u0430\u0439\u043B\u0430\
  , \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u043D\u043E\u0433\u043E \u043D\
  \u0430 \u0432\u0430\u0448\u0435\u043C \u0434\u0438\u0441\u043A\u0435, \u0432 \u0432\
  \u0430\u0448\u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0443. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\u0440\u0430\
  \u0431\u043E\u0442\u043A\u0438 \u0438\u043B\u0438\u2026"
lastmod: '2024-03-13T22:44:44.382111-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 \u0438\u0437 \u0444\u0430\u0439\u043B\u0430\
  , \u0441\u043E\u0445\u0440\u0430\u043D\u0435\u043D\u043D\u043E\u0433\u043E \u043D\
  \u0430 \u0432\u0430\u0448\u0435\u043C \u0434\u0438\u0441\u043A\u0435, \u0432 \u0432\
  \u0430\u0448\u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0443. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u043E\u0431\u0440\u0430\
  \u0431\u043E\u0442\u043A\u0438 \u0438\u043B\u0438\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
---

{{< edit_this_page >}}

## Что и Почему?

Чтение текстового файла означает получение данных из файла, сохраненного на вашем диске, в вашу программу. Программисты делают это для обработки или анализа содержимого без ручного ввода, автоматизации задач или анализа конфигурационных данных.

## Как это сделать:

```Clojure
;; Чтение всего файла как строки
(slurp "example.txt")

;; Вывод: "Привет, это содержимое вашего файла!"

;; Построчное чтение файла
(with-open [reader (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq reader)]
    (println line)))

;; Вывод:
;; Привет,
;; это твое
;; содержимое файла!
```

## Подробнее

Традиционно, чтение файлов в языках программирования было многоэтапной задачей с множеством шагов для обработки ошибок и ресурсов. В Clojure вы получаете преимущество от функции `slurp`, изящной однострочной команды для захвата всего содержимого файла. Для построчного чтения, `line-seq`, в сочетании с `with-open`, обеспечивает эффективную и безопасную обработку файлов. Также стоит отметить, что хотя `slurp` удобна, она не идеальна для больших файлов из-за ограничений памяти. Вот тут-то и сияет `line-seq`, так как она читает файл лениво, по одной строке за раз.

Альтернативы чтению файлов в Clojure включают использование `clojure.java.io/file` с функциями вроде `reader` и конструкциями вроде `with-open` для ручного управления файловым дескриптором. Здесь компромисс между простотой использования (`slurp`) и тонким контролем в сочетании с безопасностью ресурсов (`with-open` и `reader`).

С точки зрения реализации, подход Clojure базируется на классах ввода-вывода Java, так что когда вы работаете с файлами в Clojure, вы работаете с зрелыми, хорошо протестированными библиотеками Java, обёрнутыми в функциональный идиом. Всегда обращайте внимание на ресурсы: открытые файлы потребляют дескрипторы и память, так что аккуратная работа с файлами - это хорошая привычка.

## Смотрите также

- ClojureDocs для `slurp`: https://clojuredocs.org/clojure.core/slurp
- ClojureDocs для `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- Взаимодействие с Java в Clojure: https://clojure.org/reference/java_interop
- Работа с файлами в Clojure (Practical.li): https://practical.li/clojure/working-with-files.html
