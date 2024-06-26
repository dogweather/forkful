---
date: 2024-01-26 03:46:14.679589-07:00
description: "\u042F\u043A: Java \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454\
  \ \u043A\u0456\u043B\u044C\u043A\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0456\
  \u0432 \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\
  \u0441\u0435\u043B. \u041E\u0441\u044C \u043A\u043E\u0440\u043E\u0442\u043A\u0438\
  \u0439 \u0434\u0435\u043C\u043E\u043D\u0441\u0442\u0440\u0430\u0446\u0456\u0439\u043D\
  \u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u0437 `Math.round()`, `BigDecimal`\
  \ \u0456 `DecimalFormat`."
lastmod: '2024-03-13T22:44:49.071847-06:00'
model: gpt-4-0125-preview
summary: "Java \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043A\u0456\u043B\
  \u044C\u043A\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0456\u0432 \u043E\u043A\
  \u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\u0435\u043B."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

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
