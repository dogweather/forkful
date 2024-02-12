---
title:                "Интерполяция строки"
date:                  2024-01-28T23:58:51.568182-07:00
model:                 gpt-4-0125-preview
simple_title:         "Интерполяция строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Интерполяция строк позволяет нам без лишних хлопот добавлять переменные в строки. Почему? Чтобы динамично построить текст — это намного удобнее, чем старый добрый способ конкатенации строк.

## Как это сделать:
```Clojure
;; Основы с `str` и `format`
(def name "Мир")
(str "Привет, " name "!")  ; => "Привет, Мир!"

;; Использование `format`, аналогично форматированию в стиле printf
(format "До свидания, %s!" name)  ; => "До свидания, Мир!"

;; В Clojure нет встроенной интерполяции строк, как в других языках,
;; но мы можем проявить творчество с `str` и `format`.
```

## Подробнее:
Clojure - немного аскет: нет встроенной интерполяции строк. Однако, `str` и `format` являются основными инструментами для динамических строк. История происхождения? Этос простоты Clojure. Он доверяет нам самим заниматься построением строк.

В качестве альтернативы представлен мир шаблонов: `clostache` (реализация Mustache для Clojure) или `hiccup` для контекстов HTML. Они пригодятся, когда `str` и `format` кажутся слишком примитивными.

Внутри, `format` использует `String.format` из Java, что демонстрирует суперспособность Clojure к взаимодействию с Java. Так что, хотя вы не получаете сладости, у вас есть мускулы Java, когда они вам нужны.

## Смотрите также:
- Документация Clojure по `str`: https://clojuredocs.org/clojure.core/str
- Документация Clojure по `format`: https://clojuredocs.org/clojure.core/format
- репозиторий clostache на GitHub: https://github.com/fhd/clostache
- репозиторий hiccup на GitHub: https://github.com/weavejester/hiccup
