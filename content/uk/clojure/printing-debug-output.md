---
title:                "Clojure: Друк вихідних даних для виправлення помилок"
simple_title:         "Друк вихідних даних для виправлення помилок"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Чому
Програмісти часто використовують виведення на друк для відлагодження свого коду. Це дозволяє розглянути деталі виконання програми та знайти можливі помилки.

## Як це зробити
```Clojure
(defn print-debug [x]
  (println "Значення x:" x))
   
(print-debug "Hello world")
```

Виведе:
```
Значення x: Hello world
```

## Глибше пірнання
Виведення на друк дозволяє вам встановити певні кроки в вашому коді та перевірити, які значення мають змінні на цих кроках. Це особливо корисно при виконанні багатошарових обчислень або взаємодії зі зовнішніми ресурсами.

# Дивіться також
- [Відлагодження коду в Clojure](https://medium.com/ukrainian-tech-people/%D0%B2%D1%96%D0%B4%D0%BB%D0%B0%D0%B3%D0%BE%D0%B4%D0%B6%D0%B5%D0%BD%D0%BD%D1%8F-%D0%BA%D0%BE%D0%B4%D1%83-%D0%B2-clojure-d7a9039023a5)
- [Основні функції Clojure](https://clojure.org/guides/getting_started)
- [Корисні поради для програмістів Clojure](https://tech.bitplanets.com/2015/09/tips-for-clojure-beginners/)