---
title:    "Clojure: Перетворення дати в рядок"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Перетворення дати в рядок є важливою навичкою для кожного програміста, оскільки це один з ключових елементів для роботи з даними та введенням/виведенням інформації. Знання основ конвертування дат в рядок є необхідним для ефективної роботи з датами у своїх проектах.

## Як

```Clojure
(def data (java.util.Date.))

;; перетворення дати у рядок у форматі "dd/MM/yyyy"
(str (java.text.SimpleDateFormat. "dd/MM/yyyy") data)

```

Вивід: "02/03/2021"

```Clojure
;; перетворення дати у рядок за допомогою шаблону дати
(def pattern "EEE, MMM dd yyyy")
(def sdf (java.text.SimpleDateFormat. pattern))
(sdf data)

```

Вивід: "Tue, Mar 02 2021"

```Clojure
;; додавання часового поясу до рядку дати
(def zone "Europe/Kiev")
(.format (java.time.LocalDate/now)
(java.time.format.DateTimeFormatter/ofPattern "dd/MM/yyyy"))
```

Вивід: "02/03/2021 Europe/Kiev"

## Глибоке занурення

Інструкції, наведені вище, є основними для конвертування дат в рядок у Clojure. Однак, для більш детального розуміння цього процесу, важливо ознайомитися з різними вбудованими функціями та бібліотеками для роботи з датами. Наприклад, бібліотека clj-time містить багато корисних функцій для форматування та обробки дат у Clojure.

Також варто звернути увагу на форматування дат, оскільки різні країни можуть мати свої вимоги до представлення дати у рядках. Наприклад, в США можуть використовуватися місяць перед днем в "простому" форматі (MM/dd/yyyy), тоді як у багатьох країнах Європи зазвичай використовується день перед місяцем.

## Дивись також

- Офіційна документація з Clojure про конвертування дати у рядок: https://clojuredocs.org/clojure.java-time/local-date
- Бібліотека clj-time для роботи з датами у Clojure: https://github.com/clj-time/clj-time/wiki/Formatting-and-Parsing
- Стаття про форматування дат в Clojure: https://www.theserverside.com/feature/Working-with-dates-and-times-in-Clojure-functionality-and-formats