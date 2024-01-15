---
title:                "Перетворення рядка у нижній регістр"
html_title:           "Clojure: Перетворення рядка у нижній регістр"
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Які вигоди дарують перетворення рядка в нижній регістр в Clojure? Така операція допомагає полегшити порівняння та фільтрацію рядків.

## Як те зробити
```Clojure
(println (str/lower-case "HELLO WORLD")) ;; виводить "hello world"
```

```Clojure
(defn to-lower [s]
  (if s
    (str/lower-case s)
    nil))
(to-lower "CODE") ;; повертає "code"
(to-lower nil);; повертає nil
```

## Глибоке погруження

Перетворювання рядка в нижній регістр є корисною операцією, коли потрібно порівнювати різні рядки без врахування регістру. Наприклад, при роботі з данними користувачів, їх імена можуть бути написані різними варіаціями регістру (наприклад, "John", "john" або "JOHN"), але для програми це буде те ж саме ім'я. Тому, перетворення їх в нижній регістр допоможе унеможливити технічні проблеми з обробкою рядків.

## Дивись також

- [Clojure документація для функції `lower-case`](https://clojuredocs.org/clojure.string/lower-case)
- [Стаття про роботу з рядками в Clojure](https://mybroadband.co.za/news/security/4110-tutorial-manipulating-strings-in-clojure.html)