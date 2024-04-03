---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:04.738602-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Clojure \u043C\u044B \u0432 \u043E\u0441\u043D\u043E\u0432\u043D\
  \u043E\u043C \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C `Math/round`,\
  \ `Math/floor` \u0438 `Math/ceil`."
lastmod: '2024-03-13T22:44:44.340325-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure \u043C\u044B \u0432 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\
  \u043C \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C `Math/round`,\
  \ `Math/floor` \u0438 `Math/ceil`."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как это сделать:
В Clojure мы в основном используем `Math/round`, `Math/floor` и `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Для конкретных десятичных мест мы умножаем, округляем и делим:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Подробнее
До появления продвинутых языков программирования округление было ручным процессом, подумайте об абаке или бумаге. В программировании округление критически важно для представления чисел из-за ограничений точности чисел с плавающей точкой.

Альтернативы округлению включают использование класса `BigDecimal` для контроля точности или библиотеки вроде `clojure.math.numeric-tower` для расширенных математических функций. `Math/round` в Clojure зависит от функций `Math.round`, `Math/floor` и `Math/ceil` в Java, что означает наследование тех же нюансов с типами float и double.

С точки зрения реализации, когда вы округляете в Clojure, помните, что оно автоматически использует двойную точность при работе с десятичными дробями. Будьте внимательны к ошибкам округления!

## См. также
- API математики Clojure: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- API математики Java: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Понимание точности чисел с плавающей точкой: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
