---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:27.829397-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442, \u0447\u0442\u043E \u0432\u044B \u0440\u0430\u0431\u043E\
  \u0442\u0430\u0435\u0442\u0435 \u0441 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  \ \u0432 \u043C\u0438\u043D\u0438\u043C\u0430\u043B\u044C\u043D\u043E\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0435 \"Tom's Obvious, Minimal Language\", \u043F\
  \u043E\u043F\u0443\u043B\u044F\u0440\u043D\u043E\u043C \u0434\u043B\u044F \u043A\
  \u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\u043E\u043D\u043D\u044B\
  \u0445 \u0444\u0430\u0439\u043B\u043E\u0432\u2026"
lastmod: '2024-03-13T22:44:44.392285-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442, \u0447\u0442\u043E \u0432\u044B \u0440\u0430\u0431\u043E\
  \u0442\u0430\u0435\u0442\u0435 \u0441 \u0434\u0430\u043D\u043D\u044B\u043C\u0438\
  \ \u0432 \u043C\u0438\u043D\u0438\u043C\u0430\u043B\u044C\u043D\u043E\u043C \u0444\
  \u043E\u0440\u043C\u0430\u0442\u0435 \"Tom's Obvious, Minimal Language\", \u043F\
  \u043E\u043F\u0443\u043B\u044F\u0440\u043D\u043E\u043C \u0434\u043B\u044F \u043A\
  \u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\u043E\u043D\u043D\u044B\
  \u0445 \u0444\u0430\u0439\u043B\u043E\u0432\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
---

{{< edit_this_page >}}

## Что и почему?
Работа с TOML означает, что вы работаете с данными в минимальном формате "Tom's Obvious, Minimal Language", популярном для конфигурационных файлов благодаря его легкой читаемости. Программисты используют его для простого управления конфигурацией, которое сразу работает "из коробки" с понятным для человека синтаксисом.

## Как это сделать:
Чтобы работать с TOML в Clojure, вам понадобится библиотека, например, `clj-toml`. Сначала добавьте её в ваш `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Затем разберите некоторый TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'Пример TOML'")

(def parsed-config (toml/parse-string config-str))

;; Получить заголовок из разобранного TOML
(println (:title parsed-config)) ;; Вывод: Пример TOML
```

Чтобы сгенерировать TOML:

```clojure
(def data {:title "Пример TOML"})

(println (toml/generate-string data))
;; Вывод: title = "Пример TOML"
```

## Глубокое погружение
TOML был создан в 2013 году Томом Престон-Вернером, сооснователем GitHub, как более простая альтернатива YAML и JSON для конфигурационных файлов. Он направлен на ясность и предназначен для того, чтобы его могли читать люди без дополнительных инструментов.

Хотя JSON часто используется для API и веб-приложений, а YAML может быть сложным из-за ссылок и возможностей скриптов, TOML выделяется акцентом на простые, табличные структуры. Эта простота делает его особенно популярным в сообществе Rust и других современных языковых средах.

Clojure, с его акцентом на простоту и практичность, хорошо сочетается с TOML для конфигурации. `clj-toml` или альтернативные библиотеки преодолевают разрыв. Они переводят статические данные TOML в динамичный, функциональный мир Clojure.

## Смотрите также
- Репозиторий TOML на GitHub: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` на Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Документация Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- Введение в `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
