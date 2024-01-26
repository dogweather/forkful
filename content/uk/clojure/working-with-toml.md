---
title:                "Робота з TOML"
date:                  2024-01-26T04:21:19.656968-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-toml.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з TOML означає обробку даних у лаконічному форматі "Tom's Obvious, Minimal Language", що є популярним для файлів конфігурації завдяки легкій читабельності. Програмісти використовують його для простого управління конфігурацією, яке одразу "з коробки" працює з людино-зрозумілим синтаксисом.

## Як це зробити:
Щоб працювати з TOML у Clojure, вам знадобиться бібліотека, наприклад, `clj-toml`. Спочатку додайте її до вашого `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Потім розберіть якусь TOML-структуру:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; Отримаємо заголовок з розібраного TOML
(println (:title parsed-config)) ;; Вивід: TOML Example
```

Щоб згенерувати TOML:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; Вивід: title = "TOML Example"
```

## Поглиблене вивчення
TOML було створено близько 2013 року Томом Престон-Вернером, співзасновником GitHub, як спрощену альтернативу YAML та JSON для файлів конфігурації. Його мета - зрозумілість, і він призначений для того, щоб люди могли читати специфікацію без додаткових інструментів.

У той час як JSON часто використовується для API та веб-додатків, а YAML може бути складним через посилання та можливості для скриптів, TOML вирізняється своєю увагою до простих, на основі таблиць структур. Ця простота робить його особливо популярним у спільноті Rust та інших сучасних мовних середовищах.

Clojure, з його акцентом на простоті та практичності, добре пасує до TOML для конфігурації. `clj-toml` або альтернативні бібліотеки заповнюють прогалину. Вони перекладають статичні дані TOML у динамічний, функціональний світ Clojure.

## Дивіться також
- GitHub репозиторій TOML: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` на Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Документація Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- Вступ до `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)