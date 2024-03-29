---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:07.107523-07:00
description: "\u0410\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0456 \u043C\
  \u0430\u0441\u0438\u0432\u0438, \u0442\u0430\u043A\u043E\u0436 \u0432\u0456\u0434\
  \u043E\u043C\u0456 \u044F\u043A \u0445\u0435\u0448-\u0442\u0430\u0431\u043B\u0438\
  \u0446\u0456 \u0430\u0431\u043E \u0441\u043B\u043E\u0432\u043D\u0438\u043A\u0438\
  \ \u0432 PowerShell, \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0442\u044C\
  \ \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u0442\u0438 \u0434\u0430\u043D\u0456\
  \ \u0443 \u0432\u0438\u0433\u043B\u044F\u0434\u0456 \u043F\u0430\u0440 \u043A\u043B\
  \u044E\u0447-\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0449\u043E \u0440\
  \u043E\u0431\u0438\u0442\u044C \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F\
  \u2026"
lastmod: '2024-03-13T22:44:49.635281-06:00'
model: gpt-4-0125-preview
summary: "\u0410\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0456 \u043C\
  \u0430\u0441\u0438\u0432\u0438, \u0442\u0430\u043A\u043E\u0436 \u0432\u0456\u0434\
  \u043E\u043C\u0456 \u044F\u043A \u0445\u0435\u0448-\u0442\u0430\u0431\u043B\u0438\
  \u0446\u0456 \u0430\u0431\u043E \u0441\u043B\u043E\u0432\u043D\u0438\u043A\u0438\
  \ \u0432 PowerShell, \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0442\u044C\
  \ \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u0442\u0438 \u0434\u0430\u043D\u0456\
  \ \u0443 \u0432\u0438\u0433\u043B\u044F\u0434\u0456 \u043F\u0430\u0440 \u043A\u043B\
  \u044E\u0447-\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F, \u0449\u043E \u0440\
  \u043E\u0431\u0438\u0442\u044C \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F\
  \u2026"
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0430\
  \u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\
  \u0438\u0432\u0456\u0432"
---

{{< edit_this_page >}}

## Що і чому?

Асоціативні масиви, також відомі як хеш-таблиці або словники в PowerShell, дозволяють зберігати дані у вигляді пар ключ-значення, що робить отримання даних простим і ефективним. Програмісти використовують їх для зберігання пов'язаних даних разом у спосіб, який легко доступний за ключем.

## Як це зробити:

Створення та використання асоціативних масивів в PowerShell досить просте. Ось як це зробити:

**Створення асоціативного масиву:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Інженер"
```

Цей фрагмент коду створює асоціативний масив із трьома парами ключ-значення.

**Доступ до значень:**

Щоб отримати значення, зверніться до його ключа:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**Приклад виводу:**

```
Alex
```

**Додавання або зміна даних:**

Просто використовуйте ключ, щоб додати нову пару або змінити існуючу:

```PowerShell
$myAssociativeArray["location"] = "Нью-Йорк" # Додає нову пару ключ-значення
$myAssociativeArray["job"] = "Старший інженер" # Змінює існуючу пару
```

**Ітерація по асоціативному масиву:**

Проходження по ключах і значеннях виглядає так:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**Приклад виводу:**

```
name : Alex
age : 25
job : Старший інженер
location : Нью-Йорк
```

## Поглиблено

Концепція асоціативних масивів є загальною для багатьох мов програмування, зазвичай називається словником, мапою або хеш-таблицею залежно від мови. В PowerShell асоціативні масиви реалізуються як хеш-таблиці, які є досить ефективними для пошуку ключів, зберігання даних та підтримання колекції унікальних ключів.

Історично, асоціативні масиви надають засіб для управління колекціями об'єктів, де кожен елемент може бути швидко отриманий без необхідності проходження через всю колекцію, використовуючи його ключ. Ефективність отримання та модифікації даних в асоціативних масивах робить їх переважним вибором для різноманітних завдань. Однак вони мають обмеження, такі як підтримання порядку, для чого краще підійшли б словники з порядком або спеціальні об'єкти.

Незважаючи на їх обмеження, асоціативні масиви/хеш-таблиці в PowerShell надзвичайно гнучкі та потужний інструмент для скриптингу. Вони дозволяють динамічно зберігати дані та особливо корисні в конфігураціях, маніпуляціях з даними та всюди, де потрібен структурований формат даних без надмірності формального визначення класу. Просто пам'ятайте, хоча асоціативні масиви ідеально підходять для отримання доступу за ключем, якщо ваше завдання включає складні структури даних або вимагає підтримання конкретного порядку, вам може бути корисно розглянути інші типи даних або спеціальні об'єкти в PowerShell.
