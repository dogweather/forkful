---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:38:31.629303-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Конвертування рядків у нижній регістр - це процес зміни усіх літер у рядку на їх еквіваленти у нижньому регістрі. Роблять це для уніфікації даних, забезпечення відповідності форматам, або під час порівняння рядків без урахування регістру.

## Як це робити:
У Elm ми конвертуємо рядок в нижній регістр за допомогою функції `String.toLower`.

```Elm
import String

lowerCaseString : String -> String
lowerCaseString str =
    String.toLower str

-- Використання функції
sampleString : String
sampleString =
    lowerCaseString "ПривіТ, Elm!"

-- Вивід: "привіт, elm!"
```

## Заглиблення:
Історично, конвертація у нижній регістр використовувалася для спрощення порівняння та сортування тексту, де важливішим було зміст, а не форма. У деяких мовах, як-от Elm, конвертація реалізується прямо в стандартній бібліотеці через функцію `String.toLower`. Як альтернатива, в інших мовах програмування можуть бути використані регулярні вирази або власні функціонали конвертації. Зауважте, що при роботі з мовами, що мають спеціальні символи, конвертація може бути більш складною і вимагати додаткової обробки для врахування всіх відмінностей.

## Дивіться також:
- Офіційний підручник Elm по рядках: [Elm String](http://package.elm-lang.org/packages/elm-lang/core/latest/String#toLower)
- Elm пакети, що можуть бути корисними при роботі з текстом: [elm-string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- Документація MDN по `toLowerCase()`, яка може бути інформативною для порівняння: [MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
