---
title:                "Генерація випадкових чисел."
html_title:           "PowerShell: Генерація випадкових чисел."
simple_title:         "Генерація випадкових чисел."
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Згенеровання випадкових чисел - це процес отримання чисел, які випадковим чином вибираються з певного діапазону. Програмісти використовують цей підхід для створення унікальних значень для різних сценаріїв, таких як створення унікальних паролів або випадкових визначень.

## Як це зробити:
Програма відобразить повідомлення про успішне згенерування випадкового числа в діапазоні від 0 до 100. 
```PowerShell
Write-Host "Ваше випадкове число: $(Get-Random -Minimum 0 -Maximum 100)"
```

Якщо потрібно згенерувати кілька значень за один раз, можна використовувати цикл `for`. Наприклад, цей код згенерує 5 випадкових чисел:
```PowerShell
for ($i=1; $i -le 5; $i++) {
  Write-Host "Випадкове число #$i: $(Get-Random -Minimum 0 -Maximum 100)"
}
```

Крім того, можна налаштувати деякі параметри для збільшення точності та надійності генерації випадкових чисел. Наприклад, використовуючи `-SetSeed` параметр, ми можемо вказати спеціальне початкове значення для генератора випадкових чисел. Це може підвищити стійкість чисел у випадковому потоці. Детальніше про параметри можна дізнатись у розділі "## Глибоке занурення".

## Глибоке занурення:
Початки генерації випадкових чисел можна відстежити до середини ХХ століття, коли виникли перші електронні комп'ютери. Саме тоді почали використовувати різні математичні алгоритми для створення псевдовипадкових чисел. У сучасних мовах програмування, включаючи PowerShell, доступні різноманітні функції для генерації псевдовипадкових чисел. Альтернативно, можна використовувати зовнішні бібліотеки, які надають ще більший вибір параметрів та функціональності.

Якщо потрібно згенерувати унікальний ключ для захищеної інформації, можна використовувати функцію `New-Guid`, яка поверне 128-бітний унікальний ідентифікатор в форматі GUID, який використовується для ідентифікації об'єктів із високою ймовірністю суперечок. Для більш складних випадків, наприклад, для генерації унікальних файлових імен, можна використовувати поєднання різних технік, таких як хешування та дата та час.

## Дивіться також:
Якщо ви хочете дізнатись більше про функції та можливості генерації випадкових чисел у PowerShell, рекомендуємо переглянути документацію на офіційному сайті Microsoft: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1