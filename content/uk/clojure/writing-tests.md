---
title:                "Clojure: Написання тестів"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів є надзвичайно важливою частиною процесу розробки програмного забезпечення. Вони дозволяють перевірити правильність роботи коду і запобігти появі помилок в майбутньому. Також тести забезпечують більшу надійність програмного продукту і допомагають зберегти час і зусилля розробників на етапі тестування.

## Як

Написання тестів у Clojure є досить простим процесом. Використовуючи фреймворк `clojure.test`, ми можемо створювати тести за допомогою функцій `deftest` і `testing`. Наприклад:

```Clojure
(deftest add-test
  (testing "повертає правильну суму двох чисел"
    (is (= (+ 2 3) 5))))

(deftest subtract-test
  (testing "повертає правильну різницю двох чисел"
    (is (= (- 5 3) 2))))
```

Для запуску цих тестів ми можемо використовувати команду `lein test` у терміналі. В результаті ми отримаємо вивід, що підтверджує правильність роботи обох тестів.

```Clojure
Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

## Глибша прогулянка

Для написання більш складних тестів, ми можемо використовувати спеціальні функції, такі як `are`, `are-not` і `testing-vars`. З їх допомогою ми можемо перевірити, чи повертають функції правильні значення для різних вхідних даних.

Також, у Clojure існує можливість побудувати комплексні тести з використанням бібліотек, які надають додаткові функції для тестування, наприклад, `Clojure test.check` або `Midje`.

## Дивись також

- [ClojureDocs: Testing](https://clojuredocs.org/clojure.test)
- [Effective Testing with Clojure - InfoQ](https://www.infoq.com/presentations/testing-clojure/)