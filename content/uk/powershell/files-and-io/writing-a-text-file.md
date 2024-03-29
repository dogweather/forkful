---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:25.727203-07:00
description: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432\
  \ PowerShell \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0441\
  \u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u043C\u0430\u043D\
  \u0456\u043F\u0443\u043B\u044F\u0446\u0456\u044E \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u0438\u043C\u0438 \u0444\u0430\u0439\u043B\u0430\u043C\u0438, \u0449\u043E\
  \ \u0454 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\u044E \u043E\u043F\u0435\u0440\
  \u0430\u0446\u0456\u0454\u044E \u0434\u043B\u044F \u0436\u0443\u0440\u043D\u0430\
  \u043B\u044E\u0432\u0430\u043D\u043D\u044F, \u0437\u0431\u0435\u0440\u0456\u0433\
  \u0430\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445\u2026"
lastmod: '2024-03-13T22:44:49.681170-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0432\
  \ PowerShell \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454 \u0441\
  \u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u043C\u0430\u043D\
  \u0456\u043F\u0443\u043B\u044F\u0446\u0456\u044E \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u0438\u043C\u0438 \u0444\u0430\u0439\u043B\u0430\u043C\u0438, \u0449\u043E\
  \ \u0454 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\u044E \u043E\u043F\u0435\u0440\
  \u0430\u0446\u0456\u0454\u044E \u0434\u043B\u044F \u0436\u0443\u0440\u043D\u0430\
  \u043B\u044E\u0432\u0430\u043D\u043D\u044F, \u0437\u0431\u0435\u0440\u0456\u0433\
  \u0430\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445\u2026"
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
---

{{< edit_this_page >}}

## Що і чому?
Створення текстового файлу в PowerShell передбачає створення та маніпуляцію текстовими файлами, що є основною операцією для журналювання, зберігання даних та написання скриптів налаштувань. Програмісти використовують це для автоматизації системних задач, аналізу даних та інтеграції з іншими застосунками або сценаріями.

## Як:
PowerShell надає прості cmdlets для обробки файлів. Cmdlet `Out-File` та оператори перенаправлення є основними засобами для цього. Ось приклади, які ілюструють, як записувати текст у файли в різних сценаріях:

**Створення базового текстового файлу:**

Щоб створити текстовий файл та записати до нього простий рядок, ви можете використовувати:

```powershell
"Hello, World!" | Out-File -FilePath .\example.txt
```

Або еквівалентно з оператором перенаправлення:

```powershell
"Hello, World!" > .\example.txt
```

**Додавання тексту до існуючого файлу:**

Якщо ви хочете додати текст до кінця існуючого файлу, не перезаписуючи його:

```powershell
"Another line." | Out-File -FilePath .\example.txt -Append
```

Або використовуючи оператор додавання:

```powershell
"Another line." >> .\example.txt
```

**Запис декількох рядків:**

Для запису кількох рядків ви можете використовувати масив рядків:

```powershell
$lines = "Рядок 1", "Рядок 2", "Рядок 3"
$lines | Out-File -FilePath .\multilines.txt
```

**Вказівка кодування:**

Щоб вказати конкретне текстове кодування, використовуйте параметр `-Encoding`:

```powershell
"Текст з кодуванням UTF8" | Out-File -FilePath .\utfexample.txt -Encoding UTF8
```

**Використання сторонніх бібліотек:**

Хоча вбудовані cmdlets PowerShell достатні для базових операцій з файлами, більш складні завдання можуть отримати вигоду від сторонніх модулів, як-от `PowershellGet`, або інструментів, портованих для Windows, як `SED` і `AWK`. Проте, для простого запису текстового файлу, вони можуть бути надлишковими і зазвичай не потрібні:

```powershell
# Припускаючи, що більш складний сценарій виправдовує використання зовнішньої бібліотеки
# Install-Module -Name SomeComplexLibrary
# Import-Module -Name SomeComplexLibrary
# Тут можуть бути більш складні операції
```

_Зауваження: завжди враховуйте, чи виправдана складність додавання сторонньої залежності для ваших потреб._

**Приклад виводу:**

Після виконання команди базового створення файлу, перевіряючи вміст `example.txt`, показує:

```plaintext
Hello, World!
```

Для додавання тексту і потім перевірки `example.txt`:

```plaintext
Hello, World!
Another line.
```
