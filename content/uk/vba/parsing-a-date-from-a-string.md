---
title:                "Розбір дати з рядка"
date:                  2024-02-01T21:58:28.632455-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/vba/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Розбір дати з рядка в Visual Basic for Applications (VBA) полягає в перетворенні тексту, який представляє дату, у тип даних дата. Програмісти роблять це для більш ефективної роботи з датами у своїх додатках, наприклад, для порівнянь, обчислень або форматування.

## Як це зробити:

VBA пропонує простий спосіб розбору рядка на дату за допомогою функції `CDate` або функції `DateValue`. Проте, важливо, щоб рядок був у впізнаваному форматі дати.

Ось простий приклад використання `CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Розібрана дата: "; parsedDate
End Sub
```

Якщо ви запустите цей код, вивід у вікні "Терміново" (доступно через `Ctrl+G` у редакторі VBA) буде:

```
Розібрана дата: 4/1/2023 
```

Альтернативно, ви можете використовувати функцію `DateValue`, яка більш конкретна для дат (ігноруючи частину часу):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "1 квітня 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Розібрана дата за допомогою DateValue: "; parsedDate
End Sub
```

Приклад виводу для цього так само покаже в вікні "Терміново":

```
Розібрана дата за допомогою DateValue: 4/1/2023
```

Пам'ятайте, що успіх розбору залежить від відповідності формату дати у рядку налаштуванням системи або додатка.

## Поглиблений аналіз

Внутрішньо, коли VBA розбирає рядок на дату, він використовує регіональні налаштування операційної системи Windows для інтерпретації формату дати. Це важливо розуміти, тому що рядок дати, який ідеально розбирається на одній системі, може викликати помилку на іншій, якщо вони використовують різні налаштування дати/часу.

Історично, обробка дат була поширеним джерелом помилок у додатках, особливо тих, що використовуються міжнародно. Ця залежність від регіональних налаштувань у VBA є причиною, по якій деякі можуть розглядати альтернативи, такі як формат ISO 8601 (наприклад, "RRRR-MM-DD") для недвозначного представлення та розбору дат на різних системах. На жаль, VBA не підтримує ISO 8601 вроджено, і для строгого дотримання була б потрібна ручна обробка.

Для складного розбору дат, який виходить за межі того, що можуть зробити `CDate` або `DateValue`, або для забезпечення послідовного розбору незалежно від налаштувань локалі системи, програмісти можуть вдатися до функцій користувацького розбору. Це може включати розділення рядка дати на компоненти (рік, місяць, день) і створення дати за допомогою функції `DateSerial`. Інші можуть вибрати потужніші мови або бібліотеки, розроблені з урахуванням інтернаціоналізації, для подібних завдань.