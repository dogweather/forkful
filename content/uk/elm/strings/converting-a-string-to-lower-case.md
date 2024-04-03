---
date: 2024-01-20 17:38:31.629303-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0423\
  \ Elm \u043C\u0438 \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0443\u0454\u043C\u043E\
  \ \u0440\u044F\u0434\u043E\u043A \u0432 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\
  \u0435\u0433\u0456\u0441\u0442\u0440 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\
  \u043E\u0433\u043E\u044E \u0444\u0443\u043D\u043A\u0446\u0456\u0457 `String.toLower`."
lastmod: '2024-03-13T22:44:49.129315-06:00'
model: gpt-4-1106-preview
summary: "\u0423 Elm \u043C\u0438 \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0443\
  \u0454\u043C\u043E \u0440\u044F\u0434\u043E\u043A \u0432 \u043D\u0438\u0436\u043D\
  \u0456\u0439 \u0440\u0435\u0433\u0456\u0441\u0442\u0440 \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0444\u0443\u043D\u043A\u0446\u0456\u0457\
  \ `String.toLower`."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

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
