---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:15.441060-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u043F\u0435\u0440\u0448\u043E\u0457 \u043B\u0456\u0442\u0435\u0440\u0438 \u0440\
  \u044F\u0434\u043A\u0430 \u0443 PowerShell \u043D\u0430 \u0432\u0435\u043B\u0438\
  \u043A\u0443 \u0431\u0443\u043A\u0432\u0443 \u043F\u043E\u043B\u044F\u0433\u0430\
  \u0454 \u0432 \u0437\u043C\u0456\u043D\u0456 \u043F\u0435\u0440\u0448\u043E\u0433\
  \u043E \u0441\u0438\u043C\u0432\u043E\u043B\u0443 \u0434\u0430\u043D\u043E\u0433\
  \u043E \u0440\u044F\u0434\u043A\u0430 \u043D\u0430 \u0432\u0435\u043B\u0438\u043A\
  \u0443 \u043B\u0456\u0442\u0435\u0440\u0443, \u0437\u0430\u043B\u0438\u0448\u0430\
  \u044E\u0447\u0438 \u0440\u0435\u0448\u0442\u0443 \u0440\u044F\u0434\u043A\u0430\
  \ \u0431\u0435\u0437\u2026"
lastmod: '2024-03-13T22:44:49.620753-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u043F\u0435\u0440\u0448\u043E\u0457 \u043B\u0456\u0442\u0435\u0440\u0438 \u0440\
  \u044F\u0434\u043A\u0430 \u0443 PowerShell \u043D\u0430 \u0432\u0435\u043B\u0438\
  \u043A\u0443 \u0431\u0443\u043A\u0432\u0443 \u043F\u043E\u043B\u044F\u0433\u0430\
  \u0454 \u0432 \u0437\u043C\u0456\u043D\u0456 \u043F\u0435\u0440\u0448\u043E\u0433\
  \u043E \u0441\u0438\u043C\u0432\u043E\u043B\u0443 \u0434\u0430\u043D\u043E\u0433\
  \u043E \u0440\u044F\u0434\u043A\u0430 \u043D\u0430 \u0432\u0435\u043B\u0438\u043A\
  \u0443 \u043B\u0456\u0442\u0435\u0440\u0443, \u0437\u0430\u043B\u0438\u0448\u0430\
  \u044E\u0447\u0438 \u0440\u0435\u0448\u0442\u0443 \u0440\u044F\u0434\u043A\u0430\
  \ \u0431\u0435\u0437\u2026"
title: "\u0417\u0440\u043E\u0431\u0438\u0442\u0438 \u043F\u0435\u0440\u0448\u0443\
  \ \u043B\u0456\u0442\u0435\u0440\u0443 \u0440\u044F\u0434\u043A\u0430 \u0432\u0435\
  \u043B\u0438\u043A\u043E\u044E"
---

{{< edit_this_page >}}

## Що і Чому?
Перетворення першої літери рядка у PowerShell на велику букву полягає в зміні першого символу даного рядка на велику літеру, залишаючи решту рядка без змін. Програмісти часто виконують це завдання для форматування, наприклад, при підготовці тексту до відображення в інтерфейсах користувача або дотримання граматичних правил у генерованих документах.

## Як:
PowerShell, будучи багатофункціональним інструментом, дозволяє перетворювати рядок на великі букви, використовуючи прості методи без необхідності використання сторонніх бібліотек. Ось як ви можете це зробити:

```powershell
# Використання вбудованого методу .Net 'ToTitleCase' з CultureInfo
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Вивід:
```
Hello world
```

Зауважте: Цей метод перетворює на великі літери першу букву кожного слова. Якщо вам потрібно строго перетворити на велику літеру лише першу літеру рядка і залишити решту як є, ви могли б зробити щось подібне:

```powershell
# Перетворення лише першого символу рядка на велику літеру
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Вивід:
```
Hello world
```

PowerShell безпосередньо не містить простої функції для перетворення лише першої літери рядка на велику, але, комбінуючи основні методи маніпуляції з рядками, такі як `Substring(0,1).ToUpper()` та конкатенацію, ми можемо легко досягнути бажаного результату.
