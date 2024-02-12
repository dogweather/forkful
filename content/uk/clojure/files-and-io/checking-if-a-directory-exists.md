---
title:                "Перевірка наявності директорії"
aliases:
- uk/clojure/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:34.137642-07:00
model:                 gpt-4-0125-preview
simple_title:         "Перевірка наявності директорії"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Перевірка наявності директорії у Clojure полягає у верифікації присутності директорії файлової системи зсередини вашого додатку Clojure. Це завдання є критичним для операцій з файлами, щоб запобігти помилкам при читанні з або запису в директорії, яких може не бути, забезпечуючи стабільне та безпомилкове виконання коду.

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
