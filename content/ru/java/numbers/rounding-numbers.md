---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:06.526090-07:00
description: "\u041A\u0430\u043A: Java \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\
  \u0435\u0442 \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\
  \u043E\u0441\u043E\u0431\u043E\u0432 \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\
  \u0438\u044F \u0447\u0438\u0441\u0435\u043B. \u0412\u043E\u0442 \u0431\u044B\u0441\
  \u0442\u0440\u0430\u044F \u0434\u0435\u043C\u043E\u043D\u0441\u0442\u0440\u0430\u0446\
  \u0438\u044F \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u043D\u0438\u0435\u043C `Math.round()`, `BigDecimal` \u0438 `DecimalFormat`."
lastmod: '2024-03-13T22:44:44.810527-06:00'
model: gpt-4-0125-preview
summary: "Java \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 \u043D\
  \u0435\u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \u043E\u0432 \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u044F \u0447\
  \u0438\u0441\u0435\u043B."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как:
Java предлагает несколько способов округления чисел. Вот быстрая демонстрация с использованием `Math.round()`, `BigDecimal` и `DecimalFormat`.

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Использование Math.round()
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // Вывод: 123

        // Использование BigDecimal для большего контроля
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // Вывод: 123.46

        // Использование DecimalFormat
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // Вывод: 123.46
    }
}
```

## Глубже в Детали
Исторически, округление чисел было необходимо для аналоговых вычислений и перенеслось в цифровые вычисления для повышения эффективности и точности. Ошибки округления, такие как те, что возникают в арифметике с плавающей точкой, показывают, что это не тривиальная проблема — они могут накапливаться и портить расчеты в, скажем, аэрокосмических и финансовых приложениях.

Помимо `Math.round()`, у вас есть `BigDecimal`, который дает вам больший контроль над масштабом и режимом округления, и `DecimalFormat` для случаев, когда вам нужно округлить числа как часть форматирования текстового вывода. Альтернативы округлению включают обрезку до целого в меньшую сторону, округление вверх и усечение, которые являются другими способами обработки точности и обычно управляются различными методами `Math`.

В зависимости от вашего случая использования стратегия округления может варьироваться. Например, `BigDecimal` является предпочтительным для финансовых расчетов, где критически важна точность. В отличие от этого, `Math.round()` является быстрым способом для операций общего назначения, когда вы менее придирчивы к режиму округления.

## Смотрите также
- [Документация Java Math от Oracle](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [Стандарт IEEE для арифметики с плавающей точкой (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
- [Класс DecimalFormat в Java](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
