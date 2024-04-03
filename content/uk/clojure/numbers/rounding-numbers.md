---
date: 2024-01-26 03:44:03.540927-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0412\
  \ Clojure \u043C\u0438 \u043F\u0435\u0440\u0435\u0432\u0430\u0436\u043D\u043E \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u043C\u043E `Math/round`,\
  \ `Math/floor` \u0442\u0430 `Math/ceil`."
lastmod: '2024-03-13T22:44:48.644592-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure \u043C\u0438 \u043F\u0435\u0440\u0435\u0432\u0430\u0436\u043D\
  \u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u043C\
  \u043E `Math/round`, `Math/floor` \u0442\u0430 `Math/ceil`."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Як це робити:
В Clojure ми переважно використовуємо `Math/round`, `Math/floor` та `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Для конкретних десяткових місць ми множимо, округляємо та ділимо:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Поглиблений Огляд
Перед появою складних мов програмування, округлення було ручним процесом, думайте про абак або папір. У програмуванні це критично важливо для представлення чисел через обмеження точності чисел з плаваючою комою.

Альтернативи для округлення включають використання класу `BigDecimal` для контролю точності або бібліотеки, як-от `clojure.math.numeric-tower` для розширених математичних функцій. `Math/round` в Clojure базується на `Math.round`, `Math/floor` та `Math/ceil` від Java, що означає, що воно наслідує ті ж міркування щодо чисел типу float і double.

З точки зору реалізації, коли ми округлюємо в Clojure, пам'ятайте, що воно автоматично використовує подвійну точність при роботі з десятковими дробами. Будьте обережні з помилками округлення!

## Дивіться також
- API математики Clojure: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- API математики Java: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Розуміння точності чисел з плаваючою комою: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
