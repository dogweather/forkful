---
date: 2024-01-26 04:21:19.656968-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0429\u043E\u0431 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437\
  \ TOML \u0443 Clojure, \u0432\u0430\u043C \u0437\u043D\u0430\u0434\u043E\u0431\u0438\
  \u0442\u044C\u0441\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0430\
  , \u043D\u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, `clj-toml`. \u0421\u043F\
  \u043E\u0447\u0430\u0442\u043A\u0443 \u0434\u043E\u0434\u0430\u0439\u0442\u0435\
  \ \u0457\u0457 \u0434\u043E \u0432\u0430\u0448\u043E\u0433\u043E `deps.edn`."
lastmod: '2024-03-13T22:44:48.692724-06:00'
model: gpt-4-0125-preview
summary: "\u0429\u043E\u0431 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438\
  \ \u0437 TOML \u0443 Clojure, \u0432\u0430\u043C \u0437\u043D\u0430\u0434\u043E\u0431\
  \u0438\u0442\u044C\u0441\u044F \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0430, \u043D\u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, `clj-toml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

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
