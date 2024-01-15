---
title:                "Друк відладкового виводу"
html_title:           "Clojure: Друк відладкового виводу"
simple_title:         "Друк відладкового виводу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

Українською:

## Чому

Запис виводу налагодження може допомогти покращити роботу вашої програми, розв'язати проблеми та з'ясувати причини виникнення помилок. Це може бути корисно, особливо під час налагодження складних систем.

## Як

```Clojure
(defn add [a b]
  (println "Adding" a "and" b)
  (+ a b))
```

Наприклад, якщо у вас є функція "додавання", і ви хочете впевнитися, що вхідні аргументи обробляються правильно, ви можете додати вивід "Adding" разом зі значеннями вхідних змінних. Після запуску програми, у вас буде вивід: "Adding 2 і 3", що допоможе вам перевірити результат.

## Глибокий занурений

Інколи вам можуть знадобитися детальніші відомості про те, що відбувається в певній частині коду. Тоді ви можете використовувати функцію "log" з бібліотеки "clojure.tools.logging". Вона дозволяє записувати повідомлення у файл або на консоль.

```Clojure
(require '[clojure.tools.logging :as log])
(log/info "Adding" a "and" b)
```

Ви також можете використовувати рівні повідомлень, наприклад "debug", "info", "warn" або "error", щоб вибирати, які повідомлення будуть видимими у різних середовищах.

## Дивіться також

- [Офіційна документація Clojure](https://clojure.org/)
- [Функція log](https://clojuredocs.org/clojure.tools.logging/log)
- [Проблеми з налагодженням коду в Clojure](https://clojure.org/guides/repl/debugging)