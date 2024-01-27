---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Стандартна помилка (stderr) — це окремий потік виводу, призначений для логування помилок та діагностичних повідомлень, щоб не змішувати їх із основним виводом (stdout). Програмісти використовують stderr, щоб зробити обробку помилок більш гнучкою, особливо при перенаправленні виводу в файл чи інші програми.

## Як це робити:
```Clojure
;; Приклад запису в stderr у Clojure
(let [err-writer (java.io.OutputStreamWriter. *err*)]
  (.write err-writer "Це повідомлення помилки\n")
  (.flush err-writer))

;; Або використовуючи println для stderr
(clojure.core/binding [*out* *err*]
  (println "Це також повідомлення помилки"))

;; Припустимі виводи:
;; Це повідомлення помилки
;; Це також повідомлення помилки
```

## Поглиблений огляд
Historically, stderr was established to keep error messages separate from stdout, allowing users or other programs to handle only the necessary output. One alternative is logging to a file, but stderr remains useful for real-time monitoring and debugging. In Clojure, which runs on the JVM, stderr is exposed by the `*err*` writer, just like stdout is exposed by `*out*`. Both are bound to Java's `System/err` and `System/out`, respectively.

## Додатково
- [Clojure Docs](https://clojuredocs.org/) – офіційна документація з прикладами.
- [Про потоки виводу в Unix](http://www.tldp.org/LDP/abs/html/io-redirection.html) – більше про stdout і stderr в контексті Unix систем.
- [Java OutputStreamWriter](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/OutputStreamWriter.html) – для глибшого розуміння низькорівневої роботи з потоками у Java, на якій базується Clojure.
