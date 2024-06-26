---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:05.240806-07:00
description: "\u042F\u043A: Clojure, \u0437\u0430\u043B\u0438\u0448\u0430\u044E\u0447\
  \u0438\u0441\u044C \u0432\u0456\u0440\u043D\u0438\u043C \u0441\u0432\u043E\u0457\
  \u043C \u043A\u043E\u0440\u0456\u043D\u043D\u044F\u043C \u0443 \u0441\u0456\u043C\
  \u0435\u0439\u0441\u0442\u0432\u0456 Lisp, \u043F\u0440\u043E\u043F\u043E\u043D\u0443\
  \u0454 \u0431\u0430\u0433\u0430\u0442\u0438\u0439 \u043D\u0430\u0431\u0456\u0440\
  \ \u0444\u0443\u043D\u043A\u0446\u0456\u0439, \u0449\u043E \u0431\u0435\u0437\u043F\
  \u0435\u0440\u0435\u0431\u0456\u0439\u043D\u043E \u0456\u043D\u0442\u0435\u0433\u0440\
  \u0443\u044E\u0442\u044C\u0441\u044F \u0437 \u043C\u043E\u0436\u043B\u0438\u0432\
  \u043E\u0441\u0442\u044F\u043C\u0438 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\
  \u0438\u0445\u2026"
lastmod: '2024-03-13T22:44:48.636148-06:00'
model: gpt-4-0125-preview
summary: "Clojure, \u0437\u0430\u043B\u0438\u0448\u0430\u044E\u0447\u0438\u0441\u044C\
  \ \u0432\u0456\u0440\u043D\u0438\u043C \u0441\u0432\u043E\u0457\u043C \u043A\u043E\
  \u0440\u0456\u043D\u043D\u044F\u043C \u0443 \u0441\u0456\u043C\u0435\u0439\u0441\
  \u0442\u0432\u0456 Lisp, \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u0431\
  \u0430\u0433\u0430\u0442\u0438\u0439 \u043D\u0430\u0431\u0456\u0440 \u0444\u0443\
  \u043D\u043A\u0446\u0456\u0439, \u0449\u043E \u0431\u0435\u0437\u043F\u0435\u0440\
  \u0435\u0431\u0456\u0439\u043D\u043E \u0456\u043D\u0442\u0435\u0433\u0440\u0443\u044E\
  \u0442\u044C\u0441\u044F \u0437 \u043C\u043E\u0436\u043B\u0438\u0432\u043E\u0441\
  \u0442\u044F\u043C\u0438 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445\
  \ \u0432\u0438\u0440\u0430\u0437\u0456\u0432 Java."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

## Як:
Clojure, залишаючись вірним своїм корінням у сімействі Lisp, пропонує багатий набір функцій, що безперебійно інтегруються з можливостями регулярних виразів Java. Ось як ви можете їх використовувати:

### Базове співставлення
Щоб перевірити, чи рядок відповідає шаблону, використовуйте `re-matches`. Вона повертає повний збіг у разі успіху або `nil` в іншому випадку.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Пошук за шаблонами
Щоб знайти перше входження шаблону, вашим інструментом буде `re-find`:

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### Захоплення груп
Використовуйте `re-find` разом з дужками у вашому шаблоні, щоб захопити групи:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Phone: 123-4567")]
  (println "Код регіону:" area "Код:" code))
;; Вивід: Код регіону: nil Код: 123
```

### Глобальний пошук (Знайти всі збіги)
Clojure не має вбудованого глобального пошуку, як деякі мови. Натомість, використовуйте `re-seq` для отримання лінивої послідовності всіх збігів:

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### Розбиття рядків
Щоб розділити рядок за допомогою шаблону, використовуйте `clojure.string/split`:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### Заміна
Замінюйте частини рядка, що відповідають шаблону, за допомогою `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "YYYY")  ;=> "YYYY-04-01"
```

### Сторонні бібліотеки
Хоча вбудована підтримка Clojure достатня для більшості випадків, для більш складних сценаріїв розгляньте використання бібліотек, таких як `clojure.spec` для надійної валідації даних та `reagent` для реактивної маніпуляції DOM у веб-додатках з маршрутизацією та валідацією вводу на основі регулярних виразів.

```clojure
;; Приклад використання clojure.spec для валідації електронної пошти
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

Пам'ятайте, хоча регулярні вирази є потужними, вони також можуть зробити код важким для читання та підтримки. Використовуйте їх помірковано та завжди розглядайте спрощені функції маніпуляції рядками, де це можливо.
