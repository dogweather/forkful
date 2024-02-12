---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:58:56.565243-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Пошук та заміна тексту - це коли вам треба швидко обробити рядки, наприклад, виправити помилки чи змінити форматування. Програмісти це роблять, щоб зекономити час і забезпечити консистентність у своєму коді.

## Як це зробити:
```php
<?php
$originalString = "Привіт, мене звати Олексій!";
$search = "Олексій";
$replace = "Наталія";
$resultString = str_replace($search, $replace, $originalString);

echo $resultString; // Виведе: Привіт, мене звати Наталія!
?>
```
Якщо треба знайти по шаблону, використовуйте preg_replace:
```php
<?php
$originalString = "Ціна: 100 грн.";
$pattern = '/[0-9]+/';
$replacement = '200';

$resultString = preg_replace($pattern, $replacement, $originalString);

echo $resultString; // Виведе: Ціна: 200 грн.
?>
```

## Поглиблений огляд:
Пошук та заміна в тексті не новина. Вона існує з часів ранніх текстових редакторів, які дозволяли робити глобальні заміни в документах. У PHP для цього існує кілька функцій. `str_replace()` - проста і швидка функція для прямої заміни. Для складніших задач із регулярними виразами використовується `preg_replace()`. Важливо зазначити, що `preg_replace()` працює повільніше через складність обробки регулярних виразів, але натомість надає потужні можливості для пошуку за шаблоном. 

Як альтернативу, можна вживати `strtr()` для перекладу певних символів, або `mb_` функції для роботи з багатобайтовими символами, якщо ви працюєте із текстом на мовах з широким спектром символів (наприклад, японською).

Працюючи з UTF-8, завжди переконайтеся, що ви використовуєте сумісні функції (`mb_` серія функцій), інакше результат може бути неочікуваним, оскільки стандартні рядкові функції PHP не коректно оброблюють багатобайтові символи.

## Дивіться також:
- [Регулярні вирази - Довідник](https://www.regular-expressions.info/)