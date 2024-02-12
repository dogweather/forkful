---
title:                "Генерація випадкових чисел"
date:                  2024-01-27T20:35:53.830008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерація випадкових чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що та чому?

Генерація випадкових чисел в Ruby полягає у створенні чисел, які логічно не можна передбачити, що є важливим для сценаріїв на кшталт симуляцій, криптографії та ігор. Програмісти використовують випадковість для додавання непередбачуваності або імітації реальних життєвих варіативностей у свої додатки.

## Як це робити:

Ruby пропонує кілька методів для генерації випадкових чисел, переважно через клас `Random`.

### Базове випадкове число

Щоб згенерувати базове випадкове число:

```Ruby
puts rand(10) # Генерує випадкове число між 0 та 9
```

### Випадкове число в межах діапазону

Для випадкового числа в конкретному діапазоні:

```Ruby
puts rand(1..10) # Генерує випадкове число між 1 та 10
```

### Використання класу Random

Для створення повторюваної послідовності випадкових чисел, можна використати клас `Random` з заданим seed.

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # Генерує передбачуване "випадкове" число
```

### Генерація випадкового елемента масиву

Вибір випадкового елемента з масиву:

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # Випадковим чином вибирає елемент з масиву
```

### Приклад виводу:

Кожен кодовий фрагмент, виконаний вище, виробить різні результати через їхню випадкову природу. Наприклад, `rand(10)` може вивести `7`, тоді як `colors.sample` може вивести `"green"`.

## Поглиблений розгляд

Концепція генерації випадкових чисел у комп'ютерних науках є парадоксальною, оскільки комп'ютери слідують детермінованим інструкціям. Ранні методи значною мірою залежали від зовнішнього вводу для досягнення непередбачуваності. Випадковість в Ruby побудована на алгоритмі Mersenne Twister, псевдовипадковому генераторі чисел, відомому своїм великим періодом і рівномірним розподілом, що робить його дуже підходящим для застосувань, які вимагають високоякісної випадковості.

Хоча вбудовані методи Ruby добре задовольняють більшість потреб, вони можуть не вистачати для всіх криптографічних цілей, оскільки передбачуваність псевдовипадкових чисел може бути вразливістю. Для криптографічної безпеки розробники Ruby можуть досліджувати бібліотеки на кшталт `OpenSSL::Random`, які розроблені для виробництва криптографічно безпечних випадкових чисел, забезпечуючи вищу непередбачуваність для чутливих застосувань.