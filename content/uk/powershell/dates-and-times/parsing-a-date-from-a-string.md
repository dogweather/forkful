---
title:                "Розбір дати з рядка"
date:                  2024-02-03T19:16:19.147562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Розбір дати з рядка полягає в розпізнаванні та перетворенні написаних дат у текстовій формі на тип даних дати, який PowerShell може зрозуміти та з яким може працювати. Програмісти роблять це, щоб маніпулювати, форматувати, порівнювати або розраховувати дати, що є поширеними завданнями в скриптах, що стосуються роботи з файлами журналів, введенням користувача або обробкою даних.

## Як це зробити:
PowerShell робить розбір дат з рядків простим завдяки своєму аплету `Get-Date` та акселератору типу `[datetime]`, які добре працюють для стандартних форматів дат. Для більш складних або нестандартних рядків дат може бути використаний метод `[datetime]::ParseExact` для вказівки точного формату.

### Використання `Get-Date` та `[datetime]`:
```powershell
# Просте перетворення з використанням Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Приклад виведення:**
```
Субота, 01 Квітень 2023 00:00:00
```

```powershell
# Використання акселератора типу [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Приклад виведення:**
```
Субота, 01 Квітень 2023 00:00:00
```

### Використання `[datetime]::ParseExact` для нестандартних форматів:
Для форматів, що не розпізнаються автоматично, ви можете визначити точний формат для забезпечення правильного аналізу.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Приклад виведення:**
```
Субота, 01 Квітень 2023 14:00:00
```

### Використання сторонніх бібліотек
Хоча сам PowerShell є досить потужним для розбору дат, для дуже складних сценаріїв або додаткового функціоналу, ви можете розглянути .NET бібліотеки, такі як NodaTime, хоча для багатьох типових сценаріїв достатньо буде нативних можливостей PowerShell.

```powershell
# Використання NodaTime як ілюстрації, зауважте, що вам потрібно додати бібліотеку до вашого проекту
# Install-Package NodaTime -Version 3.0.5
# Використання NodaTime для аналізу дати
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Примітка до прикладу:** Вищезазначений код є концептуальною ілюстрацією. На практиці переконайтеся, що NodaTime правильно додано до вашого проекту для доступності типів і методів.
