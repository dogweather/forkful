---
title:                "Преобразование строки в верхний регистр"
date:                  2024-01-28T23:56:09.630329-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в заголовочный регистр означает написание каждой буквы с большой буквы, что обычно используется для названий или для выделения собственных имен. Программисты используют это для форматирования вывода или подготовки данных для обеспечения единообразия отображения.

## Как это сделать:
Давайте придадим тексту изюминку. В PowerShell используйте `.ToTitleCase` из `System.Globalization` для преобразования в заголовочный регистр, или простые методы вроде `.ToUpper()` или `.ToLower()` для изменения регистра.

```PowerShell
# Загружаем класс TextInfo для использования ToTitleCase
$textInfo = (Get-Culture).TextInfo

# Пример с заголовочным регистром
$titleCaseString = $textInfo.ToTitleCase("hello, powershell aficionados!")
Write-Output $titleCaseString

# Вывод: Hello, Powershell Aficionados!

# Пример с верхним регистром
$upperCaseString = "make me shout".ToUpper()
Write-Output $upperCaseString

# Вывод: MAKE ME SHOUT

# Пример с нижним регистром
$lowerCaseString = "SILENCE IS GOLDEN".ToLower()
Write-Output $lowerCaseString

# Вывод: silence is golden
```

## Глубокое изучение
Преобразование в заголовочный регистр исходит из типографской традиции, где заголовки и собственные имена начинаются с заглавных букв. В компьютерном программировании эта практика введена для визуальной стандартизации и читаемости.

Технически, `.ToTitleCase` не просто делает буквы заглавными. Он следует правилам, как, например, не преобразовывать в заглавные союзы, предлоги или артикли в некоторых контекстах. Ставим, вы не ожидали этого от однострочного кода, верно?

Существуют альтернативы: regex может выполнять замысловатые преобразования регистра, но это излишне для простых задач. К тому же, читаемость имеет значение — `.ToTitleCase`, `.ToUpper()`, и `.ToLower()` точно говорят о том, что они делают. Никаких догадок.

Один момент: будьте осторожны с правилами, специфичными для культуры, влияющими на преобразование в заголовочный регистр. Например, "i" становится "I" по-английски, но не так в других языках. Здесь `TextInfo` блестит; он учитывает культурные нюансы.

## Смотрите также
Ознакомьтесь с этими ресурсами для более глубокого изучения:

- [Microsoft Docs о ToTitleCase](https://docs.microsoft.com/ru-ru/dotnet/api/system.globalization.textinfo.totitlecase)