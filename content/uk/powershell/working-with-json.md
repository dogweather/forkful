---
title:                "Робота з json"
html_title:           "PowerShell: Робота з json"
simple_title:         "Робота з json"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## Що і чому?

Робота з JSON - це процес перетворення даних у форматі тексту в структурований формат, зрозумілий для програм. Це корисний інструмент для програмістів, оскільки дозволяє зберігати та обробляти дані у зрозумілій формі.

## Як:

```PowerShell 
# Створення JSON об’єкту за допомогою хеш-таблиці
$jsonObject = [ordered] @{
	name = 'John'
	age = 25
	city = 'Kyiv'
}

# Перетворення JSON об’єкту у рядок
$jsonString = ConvertTo-Json $jsonObject
Write-Output $jsonString
# Результат: {"name":"John","age":25,"city":"Kyiv"}

# Перетворення рядка JSON у об’єкт
$jsonObject = ConvertFrom-Json $jsonString
# Звернення до властивостей об’єкту
Write-Output $jsonObject.name
Write-Output $jsonObject.age
Write-Output $jsonObject.city
# Результат:
# John
# 25
# Kyiv
```

## Глибоке дослідження:

(1) JSON було створено в 2001 році як простий формат обміну даними. (2) Існують інші альтернативи для роботи з даними, такі як CSV або XML, проте JSON переваги завдяки своїй простоті та функціональності. (3) У PowerShell існує багато вбудованих функцій для роботи з JSON, що дозволяє легко і ефективно перетворювати дані.

## Дивіться також:

- [JSON офіційне руководство](https://www.json.org/json-en.html)
- [Робота з JSON в PowerShell](https://blog.netspi.com/working-with-json-in-powershell/)