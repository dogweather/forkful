---
date: 2024-01-20 17:52:27.243650-07:00
description: "Printing debug output is about showing intermediate results or variables\
  \ while your program is running. We do it to hunt bugs or just understand what's\u2026"
lastmod: '2024-02-25T18:49:46.218747-07:00'
model: gpt-4-1106-preview
summary: "Printing debug output is about showing intermediate results or variables\
  \ while your program is running. We do it to hunt bugs or just understand what's\u2026"
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Printing debug output is about showing intermediate results or variables while your program is running. We do it to hunt bugs or just understand what's going on inside the code.

## How to (Як це зробити):
```Clojure
(defn debug-example []
  (let [x 42]
    (println "Debug: x =" x)
    ; ... more code
))

(debug-example)
; prints: Debug: x = 42
```

```Clojure
; Advanced usage with `tap>`
(defn debug-advanced-example []
  (let [y 108]
    (tap> {"Debug y" y}) ; places the map on the tap list
    ; ... more code
  )
)

(add-tap (fn [v] (println "Tap debug:" v)))

(debug-advanced-example)
; Tap debug: {"Debug y" 108}
```

## Deep Dive (Поглиблене занурення):
Before integrated debuggers, printing to console was often the only way to debug. Now, `println` in Clojure is straightforward for simple cases, but can clutter your output. Clojure's `tap>` and `add-tap` functions provide a more sophisticated way to handle debug info.

`tap>` sends values to a 'tap list', which any number of 'tap functions' can consume. It's like a debug channel that doesn’t interfere with your main output stream. Implementing `tap>`, you can set up a separate logging window or filter messages, making it powerful for complex applications.

Real-world usage might include timestamping messages or logging to a file, all without disturbing your program's standard operation flow.

## See Also (Додаткові ресурси):
- Clojure's official guide on `println`: https://clojure.org/guides/repl/println
- Info about `tap>` and `add-tap`: https://clojure.org/reference/repl_and_main#_tap
- A broader take on debugging in Clojure: https://clojure.org/guides/debugging
