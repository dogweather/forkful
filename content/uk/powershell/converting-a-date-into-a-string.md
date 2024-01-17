---
title:                "Перетворення дати у рядок"
html_title:           "PowerShell: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Конвертування дати в рядок - це процес перетворення значення дати з одного формату в інший, зазвичай з метою використання її в програмі або на веб-сайті. Програмісти часто використовують цей метод для зручності та для уникнення помилок під час обробки дат.

## Як це зробити:

```PowerShell
$date = Get-Date 
$stringDate = $date.ToString("dd/MM/yyyy") 
Write-Output $stringDate
``` 

В результаті цього коду ми отримаємо рядок у форматі "dd/MM/yyyy", наприклад "22/11/2021". Ми можемо зміняти формат в довільному порядку, докладніше про це у розділі "Глибше".

## Глибше

Конвертування дати в рядок - це не нове поняття. Його використовували ще у старих версіях програмування, коли дати зберігалися у цифровому форматі і не були так зручними у використанні. До альтернатив конвертації дати в рядок можна віднести написання власної функції конвертації чи використання інструментів сторонніх розробників. Детальніше про формати, дозволені для використання у функції `ToString`, ви можете дізнатись на [офіційній документації Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring).

## Дивіться також:

Для більш розгорнутої інформації про конвертування дати в рядок вам можуть допомогти наступні ресурси:

- [Офіційна документація Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Швидкий підручник з PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/06-dates-and-times?view=powershell-7.1)