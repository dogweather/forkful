---
title:                "Clojure: Porivnyannya dvoh dat"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Ця стаття призначена для висвітлення порівняння двох дат в Clojure та як це може бути корисним в програмуванні.

## Як це зробити

Порівняння двох дат в Clojure дуже просте за допомогою вбудованих функцій. Спочатку ми визначимо дві змінні для дат, які потрібно порівняти:

```Clojure
(def date1 (java.util.Date. 2021 6 1))
(def date2 (java.util.Date. 2021 7 1))
```

Тепер ми можемо використовувати функцію `(.before date1 date2)`, щоб перевірити, чи `date1` передує `date2`. Вона поверне `true` у цьому випадку:

```Clojure
(.before date1 date2) ;=> true
```

Аналогічно, використовуючи функцію `(.after date1 date2)`, ми можемо перевірити, чи `date1` слідує за `date2`:

```Clojure
(.after date1 date2) ;=> false
```

Крім того, ми можемо порівняти дати за допомогою функцій `(.compareTo date1 date2)` або `(.compare date1 date2)`, які повертають відповідний цілочисельний результат:

```Clojure
(.compareTo date1 date2) ;=> -1
(.compare date1 date2) ;=> -1
```

Цей значення означає, що `date1` передує `date2`. Якщо `date1` було б пізніше `date2`, результат був би `1`. Для однакових дат результатом буде 0.

## Profound Dive

У Clojure дати представлені як об'єкти типу `java.util.Date`. Тому, перед порівнянням дат, ми можемо використати всі методи, які доступні для цього типу, наприклад, знаходження різниці між двома датами за допомогою функції `(.getTime date)`. Також є цікавим відомості, що дати можуть бути порівняні таким чином, тому що вони інтерпретуються як цілі числа.

## Дивитися також

- [Документація Clojure з вбудованими функціями дат](https://clojuredocs.org/clojure.core/date-time#clj)
- [Туторіал з порівнянням дат в Clojure](https://www.baeldung.com/clojure-comparing-dates)