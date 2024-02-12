---
title:                "Зробити першу літеру рядка великою"
aliases: - /uk/powershell/capitalizing-a-string.md
date:                  2024-02-03T19:06:15.441060-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
