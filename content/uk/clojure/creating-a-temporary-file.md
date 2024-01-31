---
title:                "Створення тимчасового файлу"
date:                  2024-01-20T17:39:49.416179-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?

Створення тимчасового файла допомагає зберігати дані, які потрібні лише короткий час. Програмісти використовують їх для тестування, безпечного вміщення чутливих даних, та уникнення засмічення системи постійними файлами.

## How to: / Як зробити:

Clojure використовує Java Interop для створення тимчасових файлів. Ось як це працює:

```Clojure
(import '(java.io File)
        '(java.nio.file Files))

(defn create-temp-file [prefix suffix]
  (.toString (Files/createTempFile prefix suffix)))

; Це створює тимчасовий файл і друкує його шлях
(println (create-temp-file "example" ".txt"))
```

Запуск цього коду дасть вам унікальний шлях до новоствореного тимчасового файла в вашій тимчасовій директорії.

## Deep Dive / Глибоке занурення:

Тимчасові файли не нові; вони були частиною програмування з самого початку, як спосіб обійти обмеження пам’яті. У Clojure, ми часто покладаємося на Java Interop для таких речей, через пов'язаність з JVM. Альтернативно, можна використати бібліотеки, такі як `clojure.java.io`, але для тимчасових файлів Java API просте та випробуване. Тимчасові файли, створені через `Files/createTempFile`, будуть автоматично видалені при виході з JVM, якщо ви не вкажете інакше.

## See Also / Дивіться також:

- JavaDoc для [Files.createTempFile](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#createTempFile(java.nio.file.Path,%20java.lang.String,%20java.lang.String,%20java.nio.file.attribute.FileAttribute...))
- Офіційне керівництво Clojure по Java Interop: [Clojure Java Interop](https://clojure.org/reference/java_interop)
- Бібліотека для роботи з файлами у Clojure: [clojure.java.io](https://clojuredocs.org/clojure.java.io)
