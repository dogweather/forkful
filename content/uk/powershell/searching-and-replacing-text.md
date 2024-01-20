---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?
Пошук та заміна тексту - це процес виявлення та заміни окремих кусків даних у програмі. Програмісти роблять це, щоб управляти і оптимізувати інформацію, що використовується в коді.

## Як це робиться:
Ось як ви можете шукати та замінювати текст у PowerShell:
```PowerShell
# Створимо приклад тексту
$example = "Привіт, світ"

# Використаємо метод Replace() для пошуку та заміни
$replaced = $example.Replace("світ", "Україна")

# Виводимо результат
Write-Output $replaced
```
Виведено:
```PowerShell
Привіт, Україна
```

## Поглиблений аналіз
Історично, процес пошуку та заміни тексту існував у програмуванні практично від початку його існування. В PowerShell пошук та заміна виконуються за допомогою вбудованого методу Replace().

Як альтернатива, ви можете скористатися регулярними виразами (RegExp), якщо маєте справу з більш складними випадками. 

Метод Replace() працює, замінюючи одну підстроку іншою. Якщо оригінальний текст не містить шуканого виразу, то текст залишиться без змін.

## Дивіться також
Посилання на додаткові ресурси:

- [MSDN: String.Replace Method in PowerShell](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)