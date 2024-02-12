---
title:                "Использование ассоциативных массивов"
aliases: - /ru/vba/using-associative-arrays.md
date:                  2024-02-01T22:04:39.321408-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование ассоциативных массивов"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/vba/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Ассоциативные массивы, часто называемые словарями в Visual Basic для приложений (VBA), позволяют программистам создавать коллекции пар ключ-значение. Эта возможность является ключевой для эффективного хранения и извлечения данных, предлагая более гибкий и интуитивно понятный способ управления данными, чем традиционные индексы массивов.

## Как это сделать:

В VBA объект `Dictionary` предоставляет функционал, аналогичный ассоциативным массивам. Прежде всего, вам нужно добавить ссылку на Microsoft Scripting Runtime, чтобы использовать его:

1. В редакторе VBA перейти в Инструменты > Ссылки...
2. Установить флажок "Microsoft Scripting Runtime" и нажать ОК.

Вот как объявить, заполнить и получить доступ к элементам в `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Добавление элементов
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' Доступ к элементам
Debug.Print sampleDictionary.Item("Name")  ' Вывод: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Вывод: 29

' Проверка существования ключа
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Ключ Occupation существует"
End If

' Удаление элементов
sampleDictionary.Remove("Occupation")

' Перебор словаря
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Подробнее

Объект `Dictionary` внутри взаимодействует с компонентами Windows Scripting Host. Таким образом, это позднепривязанный COM-объект, который был обычным способом расширения функциональности VBA в прошлом. Его использование в VBA может значительно улучшить способность языка манипулировать сложными наборами данных без наложения жесткой структуры, как это видно в традиционных массивах или диапазонах Excel.

Одним из ограничений, которые следует иметь в виду, является то, что доступ к `Dictionary` требует установки ссылки на Microsoft Scripting Runtime, что может усложнить распространение ваших проектов VBA. Альтернативы, такие как Коллекции, существуют в VBA, но они не имеют некоторых ключевых функций `Dictionary`, таких как возможность легко проверить существование ключа без вызова ошибки.

В более новых контекстах программирования языки вроде Python предлагают встроенную поддержку ассоциативных массивов (также известных как словари в Python) без необходимости добавления внешних ссылок. Эта встроенная поддержка упрощает процесс и предлагает более продвинутые функции "из коробки". Тем не менее, в рамках VBA и для конкретных приложений, направленных на автоматизацию задач в пакете Microsoft Office, использование объекта `Dictionary` остается мощным и актуальным методом для структур данных, подобных ассоциативным массивам.
