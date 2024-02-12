---
title:                "Визначення довжини рядка"
aliases:
- uk/powershell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:53.839590-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?
Отримання довжини рядка в PowerShell дозволяє визначити кількість символів у ньому. Програмісти роблять це для перевірки вхідних даних, обрізання чи форматування тексту.

## Як це зробити:
Щоб отримати довжину рядка, використовуйте властивість `.Length`. Ось як:

```PowerShell
$string = 'Привіт, світе!'
$length = $string.Length
$length
```

Вивід буде:

```
13
```

Цей код поверне довжину рядка 'Привіт, світе!', яка становить 13 символів.

## Поглиблений Розділ:
У PowerShell довжина рядка завжди вимірювалася через властивість `.Length`, подібно до інших мов програмування, як-от C# та Java, де рядки є об'єктами.

Альтернативи включають використання методів .NET класу `System.String`, наприклад `String.ToCharArray().Count`, але це зайво, коли є `.Length`.

Щодо подробиць реалізації, `.Length` - це властивість, що швидко вимірює число елементів у рядку, оскільки рядок в PowerShell - це колекція символів.

## Дивись Також:
- [Microsoft довідка про метод ToCharArray() класу String](https://docs.microsoft.com/en-us/dotnet/api/system.string.tochararray)
