---
date: 2024-01-26 04:39:15.643986-07:00
description: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0456 \u0447\u0438\
  \u0441\u043B\u0430 \u0440\u043E\u0437\u0448\u0438\u0440\u044E\u044E\u0442\u044C\
  \ \u043C\u043D\u043E\u0436\u0438\u043D\u0443 \u0434\u0456\u0439\u0441\u043D\u0438\
  \u0445 \u0447\u0438\u0441\u0435\u043B \u0434\u043E\u0434\u0430\u0442\u043A\u043E\
  \u0432\u043E\u044E \u0447\u0430\u0441\u0442\u0438\u043D\u043E\u044E, \u0443\u044F\
  \u0432\u043D\u043E\u044E \u043E\u0434\u0438\u043D\u0438\u0446\u0435\u044E 'i'. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0457\u0445 \u0443\
  \ \u0440\u0456\u0437\u043D\u043E\u043C\u0430\u043D\u0456\u0442\u043D\u0438\u0445\
  \ \u043E\u0431\u043B\u0430\u0441\u0442\u044F\u0445,\u2026"
lastmod: '2024-03-13T22:44:48.642913-06:00'
model: gpt-4-0125-preview
summary: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0456 \u0447\u0438\
  \u0441\u043B\u0430 \u0440\u043E\u0437\u0448\u0438\u0440\u044E\u044E\u0442\u044C\
  \ \u043C\u043D\u043E\u0436\u0438\u043D\u0443 \u0434\u0456\u0439\u0441\u043D\u0438\
  \u0445 \u0447\u0438\u0441\u0435\u043B \u0434\u043E\u0434\u0430\u0442\u043A\u043E\
  \u0432\u043E\u044E \u0447\u0430\u0441\u0442\u0438\u043D\u043E\u044E, \u0443\u044F\
  \u0432\u043D\u043E\u044E \u043E\u0434\u0438\u043D\u0438\u0446\u0435\u044E 'i'. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0457\u0445 \u0443\
  \ \u0440\u0456\u0437\u043D\u043E\u043C\u0430\u043D\u0456\u0442\u043D\u0438\u0445\
  \ \u043E\u0431\u043B\u0430\u0441\u0442\u044F\u0445,\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u0438\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
---

{{< edit_this_page >}}

## Що та чому?
Комплексні числа розширюють множину дійсних чисел додатковою частиною, уявною одиницею 'i'. Програмісти використовують їх у різноманітних областях, включаючи обробку сигналів, теорію електромагнетизму та фрактали, де розрахунки, що включають квадратний корінь з від'ємного числа, є звичайними.

## Як:
Clojure надає вбудовану підтримку для комплексних чисел через утиліту класу `clojure.lang.Numbers`. Використовуйте `complex` для створення комплексних чисел та виконання арифметичних дій.

```clojure
;; Створення комплексних чисел
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Додавання
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Віднімання
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Множення
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Ділення
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Спряжене
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Поглиблено
Комплексні числа були формалізовані математиками, такими як Гаус і Ейлер, у 18 столітті. Хоча спочатку вони зустрічали скептицизм, згодом вони стали критично важливими в сучасній науці та інженерії. Clojure не має власного типу для комплексних чисел, на відміну від деяких мов (наприклад, Python), але включений Java інтероп може обробляти необхідні операції через клас `clojure.lang.Numbers`.

`java.lang.Complex` у Java є потужною альтернативою, яка надає більше можливостей та потенційних оптимізацій. Інтероперабельність хоста в Clojure полегшує роботу з Java-бібліотеками.

Під кришкою арифметика комплексних чисел включає додавання та множення дійсних та уявних частин, з ключовим правилом, що `i^2 = -1`. Ділення комплексних чисел може бути складнішим, зазвичай потребуючи спряження, щоб уникнути ділення на комплексні числа.

## Див. також
- ClojureDocs, для швидкого довідника: https://clojuredocs.org/
- Java API для `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- Сторінка Вікіпедії про комплексні числа для математично цікавих: https://en.wikipedia.org/wiki/Complex_number
