---
title:                "Округлення чисел"
aliases:
- /uk/clojure/rounding-numbers/
date:                  2024-01-26T03:44:03.540927-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Округлення чисел полягає в тому, щоб скорегувати число до найближчого цілого або до певної десяткової точності. Ми округляємо числа, щоб спростити їх для зручності людей, зменшити обчислювальне навантаження або задовільнити певні числові вимоги.

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
