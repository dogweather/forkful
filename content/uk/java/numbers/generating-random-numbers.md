---
title:                "Генерація випадкових чисел"
aliases:
- /uk/java/generating-random-numbers/
date:                  2024-01-27T20:34:23.314002-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерація випадкових чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?

Генерація випадкових чисел полягає у створенні непередбачуваних послідовностей або окремих значень у визначеному діапазоні. Програмісти використовують цю техніку з різних причин, включаючи симуляції, ігри, застосунки безпеки та методи вибірки для тестування алгоритмів в різних умовах.

## Як це зробити:

У Java генерація випадкових чисел може бути досягнута за допомогою класу `Random` з пакету `java.util`, або класів `ThreadLocalRandom` та `SecureRandom` для специфічних випадків використання. Наступні приклади ілюструють, як використовувати ці класи.

### Використання класу `Random`
Клас `Random` пропонує спосіб генерації простих псевдовипадкових чисел.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Створення об'єкта Random

        int randInt = rand.nextInt(50); // Генерує випадкове ціле від 0 до 49
        double randDouble = rand.nextDouble(); // Генерує випадкове дробове число між 0.0 та 1.0
        boolean randBoolean = rand.nextBoolean(); // Генерує випадкове булеве значення
        
        System.out.println("Випадкове Int: " + randInt);
        System.out.println("Випадкове Double: " + randDouble);
        System.out.println("Випадкове Boolean: " + randBoolean);
    }
}
```

### Використання класу `ThreadLocalRandom`
Для паралельних застосунків `ThreadLocalRandom` є ефективнішим, ніж `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // Від 1 до 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // Від 1.0 до 10.0
        
        System.out.println("Випадкове Int: " + randInt);
        System.out.println("Випадкове Double: " + randDouble);
    }
}
```

### Використання класу `SecureRandom`
Для криптографічних операцій `SecureRandom` забезпечує вищий рівень безпеки.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Заповнює масив bytes випадковими безпечними числами
        
        System.out.println("Безпечні випадкові байти:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Поглиблений аналіз

Генерація випадкових чисел значно еволюціонувала з ранніх днів комп'ютерної ери. Клас `Random` Java використовує лінійну конгруентну формулу для генерації псевдовипадкових чисел, які є детермінованими та не підходять для застосунків з високим рівнем безпеки. Це призвело до появи `SecureRandom`, який використовує більш складні алгоритми (наприклад, SHA1PRNG) для виробництва криптографічно стійких випадкових чисел.

Однак, `Random` та `SecureRandom` мають свої недоліки, такі як погіршення продуктивності в багатопоточних середовищах. Клас `ThreadLocalRandom` був введений у Java 7 для вирішення цієї проблеми, надаючи генератори випадкових чисел на основі локальних потоків, значно покращуючи продуктивність у паралельних застосунках.

Хоча ці класи покривають більшість потреб, для дуже великих або спеціалізованих вимог розробники можуть досліджувати додаткові бібліотеки або розробляти користувацькі рішення. Важливо вибрати правильний підхід на основі потреб у безпеці та вимог до продуктивності конкретного випадку використання.
