---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:03.457019-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Java \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F\
  \ \u0441\u043B\u0443\u0447\u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\
  \u043B \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u0440\u0435\u0430\
  \u043B\u0438\u0437\u043E\u0432\u0430\u043D\u0430 \u0441 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043A\u043B\u0430\u0441\
  \u0441\u0430 `Random` \u0438\u0437 \u043F\u0430\u043A\u0435\u0442\u0430 `java.util`,\
  \ \u0430 \u0442\u0430\u043A\u0436\u0435 \u043A\u043B\u0430\u0441\u0441\u043E\u0432\
  \u2026"
lastmod: '2024-03-13T22:44:44.812390-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Java \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\
  \u043B\u0443\u0447\u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B\
  \ \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u0440\u0435\u0430\u043B\
  \u0438\u0437\u043E\u0432\u0430\u043D\u0430 \u0441 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043A\u043B\u0430\u0441\u0441\
  \u0430 `Random` \u0438\u0437 \u043F\u0430\u043A\u0435\u0442\u0430 `java.util`, \u0430\
  \ \u0442\u0430\u043A\u0436\u0435 \u043A\u043B\u0430\u0441\u0441\u043E\u0432 `ThreadLocalRandom`\
  \ \u0438 `SecureRandom` \u0434\u043B\u044F \u043A\u043E\u043D\u043A\u0440\u0435\u0442\
  \u043D\u044B\u0445 \u0441\u043B\u0443\u0447\u0430\u0435\u0432 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u044F."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\u043B\u0443\u0447\
  \u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

## Как это сделать:
В Java генерация случайных чисел может быть реализована с использованием класса `Random` из пакета `java.util`, а также классов `ThreadLocalRandom` и `SecureRandom` для конкретных случаев использования. Следующие примеры показывают, как использовать эти классы.

### Использование класса `Random`
Класс `Random` предлагает способ генерации простых псевдослучайных чисел.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Создание объекта Random

        int randInt = rand.nextInt(50); // Генерирует случайное целое число от 0 до 49
        double randDouble = rand.nextDouble(); // Генерирует случайное число типа double между 0.0 и 1.0
        boolean randBoolean = rand.nextBoolean(); // Генерирует случайное логическое значение
        
        System.out.println("Случайное целое: " + randInt);
        System.out.println("Случайное число double: " + randDouble);
        System.out.println("Случайное логическое значение: " + randBoolean);
    }
}
```

### Использование класса `ThreadLocalRandom`
Для параллельных приложений `ThreadLocalRandom` работает эффективнее, чем `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // От 1 до 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // От 1.0 до 10.0
        
        System.out.println("Случайное целое: " + randInt);
        System.out.println("Случайное число double: " + randDouble);
    }
}
```

### Использование класса `SecureRandom`
Для криптографических операций `SecureRandom` обеспечивает более высокий уровень безопасности.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Заполняет массив bytes случайными безопасными числами
        
        System.out.println("Защищенные случайные байты:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Глубокое погружение
Генерация случайных чисел значительно эволюционировала с начала эры компьютерных технологий. Класс `Random` в Java использует линейное конгруэнтное уравнение для генерации псевдослучайных чисел, которые являются детерминированными и не подходят для приложений с высокими требованиями к безопасности. Это привело к появлению `SecureRandom`, который использует более сложные алгоритмы (например, SHA1PRNG) для производства криптографически стойких случайных чисел.

Однако `Random` и `SecureRandom` имеют свои недостатки, такие как снижение производительности в многопоточных средах. Класс `ThreadLocalRandom` был введен в Java 7 для решения этой проблемы, предоставляя генераторы случайных чисел на уровне потоков, что значительно улучшило производительность в параллельных приложениях.

Хотя эти классы покрывают большинство потребностей, для очень масштабных или специализированных требований разработчики могут исследовать дополнительные библиотеки или разрабатывать собственные решения. Важно выбрать правильный подход, исходя из потребностей в безопасности и требований к производительности конкретного случая использования.
