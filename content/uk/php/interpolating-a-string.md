---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:26.309335-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Інтерполяція рядків у PHP — це вставка змінних прямо у текст рядка. Це зручно для швидкого формування динамічного тексту без використання конкатенації.

## Як це зробити:
```PHP
$name = 'Максим';
$age = 25;

// Ось як можна використовувати інтерполяцію:
$greeting = "Привіт, $name! Тобі $age років.";
echo $greeting; // Виведе: Привіт, Максим! Тобі 25 років.

// Використання фігурних дужок для складніших виразів:
$item = 'кава';
$greeting = "Чи хочеш трохи {$item}ення?";
echo $greeting; // Виведе: Чи хочеш трохи каваення?
```

## Глибше занурення:
Інтерполяція рядків в PHP з'явилася ще в PHP 3, щоб спростити роботу з текстом. Є альтернативи: конкатенація рядків за допомогою оператора `.` та використання `sprintf` для більш складного форматування.

```PHP
$announcement = 'семінар з PHP'; 
$time = '17:00';

// Конкатенація:
echo 'Не забудьте про ' . $announcement . ' о ' . $time . '!';

// Sprintf:
echo sprintf('Не забудьте про %s о %s!', $announcement, $time);
```

При інтерполяції важливо використовувати подвійні кавички ("), оскільки одинарні (') не впізнають змінні в середині рядка. У рядкових літералах зі складними змінними або ключового слова об'єкта використовуються фігурні дужки (`{}`).

## Дивіться також:
- [PHP: Strings](https://www.php.net/manual/en/language.types.string.php)
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php)
- [PHP: String Operators](https://www.php.net/manual/en/language.operators.string.php)
