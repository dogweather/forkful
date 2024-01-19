---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Парсинг HTML - це процес, під час якого програма читає HTML-код і розуміє його структуру. Це лягає в основу багатьох веб-скраперів та інших ємних застосунків, щоб надати корисну інформацію із сложної структури HTML.

## Як це робити:

PowerShell маєат все необхідне для парсингу HTML. Спрощений приклад:

```PowerShell
# Встановлюємо бібліотеку HtmlAgilityPack
Install-Package -Name HtmlAgilityPack

# Імпортуємо бібліотеку
Add-Type -Path 'C:\path\to\HtmlAgilityPack.dll'

# Створюємо новий об'єкт парсера
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Завантажуємо HTML
$doc.Load('C:\path\to\your.html')

# Доступ до елементів HTML
$doc.DocumentNode.SelectSingleNode('//title').InnerText
```

Це код виведе вміст тега `<title>` з вказаного файлу HTML.

## Занурення в глибини:

Перед розробкою PowerShell, парсинг HTML зазвичай передбачав використання низькорівневих мов програмування, що було в значної мірі занадто складно. Інструменти, такі як HtmlAgilityPack, значно спростили процес, дозволяючи перетворити HTML в об'єкти .NET, які можна легко маніпулювати.

Як альтернативи, можна використовувати інші мови програмування, як-от Python або Javascript, що мають великий вибір бібліотек для парсингу HTML.

Використання Regex для парсингу HTML є поганою практикою, оскільки це призводить до проблем зі стійкістю та відсутністью гнучкості.

## Див. також:
1. HtmlAgilityPack: https://www.nuget.org/packages/HtmlAgilityPack/
2. Парсинг HTML з Python: https://docs.python.org/3/library/html.parser.html
3. Парсинг HTML з Javascript: https://developer.mozilla.org/uk/docs/Web/API/DOMParser
4. Погана практика: парсинг HTML з Regex: https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454