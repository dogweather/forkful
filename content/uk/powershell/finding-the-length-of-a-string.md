---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# PowerShell: Як знайти довжину рядка

## Що і Навіщо?
Визначення довжини рядка – це процес виявлення кількості символів у певному тексті. Програмісти це роблять, щоб контролювати і маніпулювати даними, проводити в пам'яті подальші операції.

## Як це зробити:
Ось як ми можемо визначити довжину рядка в PowerShell:

```PowerShell
$string = 'Привіт світ!'
$length = $string.Length
$length
```

Якщо ви запустите цей код, його вихід буде довжиною вказаного рядка.

```PowerShell
13
```

## Поглиблення
**Історичний контекст:** Визначення довжини рядка було основою текстових обробок ще з початку програмування.

**Альтернативи:** В PowerShell, ви також можете використовувати цикл `foreach` для визначення довжини рядка, але це повільніше за використання Length.

**Деталі імплементації:** `Length` є властивістю об'єкта System.String у .NET. Вона повертає кількість символів, представлених об'єктом String.

## Дивись також
1. [String.Length Property (Майкрософт офіційна документація)](https://docs.microsoft.com/dotnet/api/system.string.length)
2. [Learn how to use PowerShell (Майкрософт офіційний курс)](https://docs.microsoft.com/powershell/scripting/learn?view=powershell-7.1)