---
aliases:
- /uk/clojure/working-with-toml/
date: 2024-01-26 04:21:19.656968-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u043E\u0431\u0440\u043E\u0431\u043A\u0443 \u0434\u0430\u043D\
  \u0438\u0445 \u0443 \u043B\u0430\u043A\u043E\u043D\u0456\u0447\u043D\u043E\u043C\
  \u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456 \"Tom's Obvious, Minimal Language\"\
  , \u0449\u043E \u0454 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u043C\
  \ \u0434\u043B\u044F \u0444\u0430\u0439\u043B\u0456\u0432 \u043A\u043E\u043D\u0444\
  \u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457 \u0437\u0430\u0432\u0434\u044F\u043A\
  \u0438 \u043B\u0435\u0433\u043A\u0456\u0439\u2026"
lastmod: 2024-02-18 23:08:59.864532
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u043E\u0431\u0440\u043E\u0431\u043A\u0443 \u0434\u0430\u043D\
  \u0438\u0445 \u0443 \u043B\u0430\u043A\u043E\u043D\u0456\u0447\u043D\u043E\u043C\
  \u0443 \u0444\u043E\u0440\u043C\u0430\u0442\u0456 \"Tom's Obvious, Minimal Language\"\
  , \u0449\u043E \u0454 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u043C\
  \ \u0434\u043B\u044F \u0444\u0430\u0439\u043B\u0456\u0432 \u043A\u043E\u043D\u0444\
  \u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457 \u0437\u0430\u0432\u0434\u044F\u043A\
  \u0438 \u043B\u0435\u0433\u043A\u0456\u0439\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
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
