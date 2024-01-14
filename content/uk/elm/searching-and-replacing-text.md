---
title:                "Elm: Пошук та заміна тексту"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Найчастіше, нам потрібно виконувати пошук та заміну деякого тексту у великих проектах або розробках. Elm мова програмування дозволяє це робити швидко та ефективно, що дуже корисно для нашої роботи.

## Як це зробити

Для пошуку та заміни тексту в Elm ми можемо використовувати функцію `String.replace` у комбінації з регулярними виразами (RegExp).

```Elm
import String

-- Змінна з початковим рядком
originalString = "Привіт, Elm!"

-- Результат заміни рядка Elm на рядок JavaScript
replacedString = String.replace (RegExp.fromText "Elm") "JavaScript" originalString

-- Вивід результату на екран
main = replacedString
```

Вищезазначений код виведе на екран рядок "Привіт, JavaScript!".

Ми також можемо використовувати функцію `String.replace` у комбінації з функцією `String.contains` для заміни всіх входжень певного тексту у рядку.

```Elm
import String

-- Змінна з початковим рядком
originalString = "Привіт, світ! Привіт, Elm!"

-- Результат заміни рядка Привіт на Хаюхай
replacedString = List.foldl
		( \_ str -> String.replace (RegExp.fromText "Привіт") "Хаюхай" str
		)
		originalString
		(Text.words originalString)

-- Вивід результату на екран
main = replacedString
```

Вищезазначений код виведе на екран рядок "Хаюхай, світ! Хаюхай, Elm!".

## Глибокий занурення

У Elm також є можливість використовувати регулярні вирази з допомогою пакету `elm-tools/parser`. Це дозволяє більш гнучко працювати з рядками та виконувати складніші задачі заміни тексту.

```Elm
import Regex exposing (replace, regex)
import Parser exposing ((|.),word, repeat, spaces, succeed, run)

-- Парсер для заміни рядка Elm на JavaScript
parser = succeed (replace (regex "Elm") "JavaScript")
	|. spaces
	|. word "Привіт"
	|. spaces
	|. repeat (word "світ") Nothing
	|. spaces

originalString = "Привіт світ. Elm Привіт світ, Elm!"

-- Результат заміни рядка Elm на JavaScript
replacedString = run parser originalString

-- Вивід результату на екран
main = case replacedString of
	Ok str -> str
	Err err -> "Помилка: " ++ err
```

Вищезазначений код виведе на екран рядок "Привіт світ. JavaScript Привіт світ, JavaScript!".

## Дивись також

- [Документація Elm](https://guide.elm-lang.org/)
- [Експлуатуйте потужність регулярних виразів у Elm](https://www.elm-tutorial.org/en/08-custom-types/02-regexp.html)