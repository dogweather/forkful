---
title:                "Склеивание строк"
aliases:
- ru/powershell/concatenating-strings.md
date:                  2024-01-28T23:56:40.395297-07:00
model:                 gpt-4-0125-preview
simple_title:         "Склеивание строк"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Объединение строк или конкатенация похоже на создание поезда из слов. Мы делаем это для того, чтобы сшить вместе текстовые значения, создавая фразы, предложения или что-то еще, где отдельные строки должны объединиться и стать одним целым.

## Как это сделать:
Перейдем непосредственно к делу:

```PowerShell
# Использование оператора '+'
$greeting = 'Привет, ' + 'Мир!'
$greeting # Вывод: Привет, Мир!

# Через интерполяцию строк
$name = 'Джейн'
$welcomeMessage = "Привет, $name, рад встрече с тобой!"
$welcomeMessage # Вывод: Привет, Джейн, рад встрече с тобой!

# С использованием оператора -f (оператор форматирования)
$city = 'Нью-Йорк'
$visitMessage = 'Добро пожаловать в {0}!' -f $city
$visitMessage # Вывод: Добро пожаловать в Нью-Йорк!

# StringBuilder для сложных сценариев (немного избыточно для простых задач)
$textBuilder = New-Object System.Text.StringBuilder
[void]$textBuilder.Append('PowerShell ')
[void]$textBuilder.Append('потрясающий.')
$textBuilder.ToString() # Вывод: PowerShell потрясающий.
```

## Подробнее
Исторически, конкатенация строк была немного грубой в более ранних языках программирования - представьте это как использование ленты для склеивания предложений. В PowerShell это прогулка по парку.

Существуют разные способы выполнения этой задачи. Оператор '+' прост в использовании, но может быть медленным при работе с большим количеством строк. Интерполяция строк с "$variable" более чистая и великолепно подходит для вставки переменных в строки. Оператор форматирования '-f' блестит в сценариях шаблонизации.

Что касается производительности - если вы объединяете столько строк, сколько в сочинении, вам понадобится что-то более мощное. Вступает `StringBuilder`. Он не выполняет конкатенацию сразу; вместо этого он сплетает ваши строки вместе по запросу, экономя время и память для крупных задач конкатенации.

## Смотрите также
- [О Join](https://docs.microsoft.com/ru-ru/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.3)
- [О автоматических переменных](https://docs.microsoft.com/ru-ru/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.3) (смотрите `$OFS`)
- Для получения дополнительных сведений о форматировании строк, ознакомьтесь с [Композитным форматированием](https://docs.microsoft.com/ru-ru/dotnet/standard/base-types/composite-formatting).
- А если вы готовы к этому, вот подробная информация о [StringBuilder](https://docs.microsoft.com/ru-ru/dotnet/api/system.text.stringbuilder?view=net-6.0).
