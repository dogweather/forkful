---
title:                "Округлення чисел"
date:                  2024-01-26T03:46:14.679589-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Округлення чисел означає їх коригування до вказаного ступеня точності. Програмісти роблять це, щоб спростити числа для зручності читання, відповідати певним специфікаціям, або для забезпечення виконання обчислень у певних межах, наприклад, уникнути помилок точності при роботі з плаваючою комою.

## Як:
Java пропонує кілька способів округлення чисел. Ось короткий демонстраційний приклад з `Math.round()`, `BigDecimal` і `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Використовуючи Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Вивід: 123

        // Використовуючи BigDecimal для більшого контролю
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Вивід: 123.46

        // Використовуючи DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Вивід: 123.46
    }
}
```

## Поглиблено
Історично, округлення чисел було необхідним для аналогових обчислень і перейшло до цифрових обчислень для підвищення ефективності та точності. Помилки округлення, як-от при роботі з плаваючою комою, показують, що це не тривіальна проблема — вони можуть накопичувати помилки в обчисленнях, наприклад, в аерокосмічній галузі та фінансових застосуваннях.

Окрім `Math.round()`, у вас є `BigDecimal`, який дає вам більший контроль над масштабом та режимом округлення, а також `DecimalFormat`, коли вам потрібно округлити числа як частину форматування текстового виводу. Альтернативами округленню є методи до нижнього скраю, верхнього скраю, та обрізання, які є різними способами обробки точності і зазвичай обробляються різними методами `Math`.

Залежно від вашого випадку використання, стратегія округлення може варіюватися. Наприклад, `BigDecimal` є ідеальним для фінансових обчислень, де критично важлива точність. У контрасті, `Math.round()` є швидким способом для загальноцільових операцій, де ви менше переймаєтеся режимом округлення.

## Дивіться також
- [Документація Java Math від Oracle](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [IEEE Стандарт для арифметики з плаваючою комою (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [Клас DecimalFormat у Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)