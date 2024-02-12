---
title:                "Запис до стандартної помилки"
aliases:
- uk/clojure/writing-to-standard-error.md
date:                  2024-02-03T19:33:15.694480-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запис до стандартної помилки"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Запис в стандартну помилку (stderr) стосується перенаправлення повідомлень про помилки та діагностики в потік stderr, окремо від стандартного виводу (stdout). Програмісти роблять це для розмежування звичайного виводу програми від повідомлень про помилки, що дозволяє ефективніше налагоджувати та логувати.

## Як це зробити:
У Clojure ви можете писати в stderr, використовуючи потік `*err*`. Ось простий приклад:

```clojure
(.write *err* "Це повідомлення про помилку.\n")
```

Зауважте, що після написання повідомлення, вам слід очистити потік, щоб переконатись, що повідомлення відразу виводиться:

```clojure
(flush)
```

Приклад виводу в stderr:
```
Це повідомлення про помилку.
```

Якщо ви обробляєте виключення, вам може знадобитися друкувати траси стека в stderr. Для цього використовуйте `printStackTrace`:

```clojure
(try
  ;; Код, що може сгенерувати виключення
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Для більш структурованого логування помилок могли бути налаштовані сторонні бібліотеки, як-от `timbre`, для логування в stderr. Ось основна настройка та використання:

Спочатку додайте `timbre` до ваших залежностей. Потім налаштуйте його на використання stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Вимкнути логування stdout
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Відключити логування до файлу
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Увімкнути stderr для помилок

(timbre/error "Під час обробки вашого запиту сталася помилка.")
```

Це спрямує повідомлення про помилки на рівні помилок в stderr, роблячи їх відмінними від стандартного виводу програми.
