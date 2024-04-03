---
date: 2024-01-20 17:39:49.416179-07:00
description: "How to: / \u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Clojure\
  \ \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454 Java\
  \ Interop \u0434\u043B\u044F \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\u0438\u0445 \u0444\u0430\u0439\
  \u043B\u0456\u0432. \u041E\u0441\u044C \u044F\u043A \u0446\u0435 \u043F\u0440\u0430\
  \u0446\u044E\u0454."
lastmod: '2024-03-13T22:44:48.686538-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454 Java Interop \u0434\u043B\u044F \u0441\u0442\u0432\u043E\u0440\u0435\u043D\
  \u043D\u044F \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\u0438\u0445 \u0444\
  \u0430\u0439\u043B\u0456\u0432."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

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
