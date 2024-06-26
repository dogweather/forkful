---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:34.137642-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Clojure, \u0431\
  \u0443\u0434\u0443\u0447\u0438 \u043C\u043E\u0432\u043E\u044E JVM, \u043C\u043E\u0436\
  \u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\
  \u0442\u0438 \u043A\u043B\u0430\u0441 `java.io.File` \u0437 Java \u0434\u043B\u044F\
  \ \u0446\u0456\u0454\u0457 \u043C\u0435\u0442\u0438. \u0412\u0430\u043C \u043D\u0435\
  \ \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u0430 \u0436\u043E\u0434\u043D\u0430\
  \ \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u044F \u0431\u0456\u0431\u043B\u0456\
  \u043E\u0442\u0435\u043A\u0430 \u0434\u043B\u044F \u0442\u0430\u043A\u043E\u0457\
  \u2026"
lastmod: '2024-03-13T22:44:48.677898-06:00'
model: gpt-4-0125-preview
summary: "Clojure, \u0431\u0443\u0434\u0443\u0447\u0438 \u043C\u043E\u0432\u043E\u044E\
  \ JVM, \u043C\u043E\u0436\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043A\u043B\u0430\u0441 `java.io.File`\
  \ \u0437 Java \u0434\u043B\u044F \u0446\u0456\u0454\u0457 \u043C\u0435\u0442\u0438\
  ."
title: "\u041F\u0435\u0440\u0435\u0432\u0456\u0440\u043A\u0430 \u043D\u0430\u044F\u0432\
  \u043D\u043E\u0441\u0442\u0456 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0456\
  \u0457"
weight: 20
---

## Як зробити:
Clojure, будучи мовою JVM, може використовувати клас `java.io.File` з Java для цієї мети. Вам не потрібна жодна стороння бібліотека для такої базової операції. Ось як ви можете це зробити:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; Приклад використання
(println (directory-exists? "/path/to/your/directory")) ;; true або false
```

Ця функція, `directory-exists?`, приймає шлях до директорії як рядок і повертає `true`, якщо директорія існує, та `false` в іншому випадку. Це досягається шляхом створення об’єкта `File` із шляхом до директорії та подальшого виклику методу `.exists` для цього об’єкта.

Додатково до прямого використання Java, ви можете використовувати бібліотеки Clojure, які абстрагують деякі шаблони Java. Однією з таких бібліотек є `clojure.java.io`. Однак, для перевірки існування директорії, ви все одно використовували б клас `File`, але можливо знайдете бібліотеку корисною для інших операцій з файлами. Приклад:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; Приклад використання
(println (directory-exists?-clojure "/another/path/to/check")) ;; true або false
```

Ця версія дуже схожа, але використовує функцію `io/file` Clojure для створення об'єкта `File`. Цей метод природніше інтегрується в бази коду Clojure, використовуючи бібліотеку Clojure для операцій введення-виведення, замість прямої взаємодії з класами Java.
