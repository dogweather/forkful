---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Створення тимчасового файлу - це процес, при якому генерується окремий, короткочасний файл для міжсезонного чи тимчасового зберігання даних. Програмісти роблять це, коли є потреба в обробці великих об'ємів інформації, що не повинна залишитися постійно в системі.

## Як це робити:

Створення тимчасового файлу в Clojure може виглядати так:

```Clojure
(require '[clojure.java.io :as io])

(defn create-temp-file [prefix suffix]
  (let [temp-file (java.io.File/createTempFile prefix suffix)]
    (println (.getAbsolutePath temp-file))
    (.deleteOnExit temp-file)))
```

Вказавши префікс і суфікс, ми генеруємо унікальний тимчасовий файл.

## Поглиблено:

Створення тимчасового файлу - старий метод, що датується до часів, коли розмір пам'яті був ограничений. Сьогодні цей метод все ще користується популярністю, особливо при обробці великих файлів.

Альтернативою може бути використання in-memory databases або створення постійного файлу, що потім видаляється. Обідва методи мають свої переваги та недоліки.

Під капотом, `java.io.File/createTempFile` інтерфейс використовує системні засоби операційної системи для генерації унікальних імен файлів.

## Див. також:

За більш детальною інформацією рекомендую звернутися до [Java 7 Documentation for File.createTempFile](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)), та [Clojure Cookbook: Temporary Files](https://clojure-cookbook.com/).