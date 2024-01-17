---
title:                "Знаходження довжини рядка"
html_title:           "Elm: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?

Знаходження довжини рядка - це процес, при якому визначається кількість символів у текстовому рядку. Це корисна операція для програмістів, оскільки дозволяє визначити кількість знаків у рядку, що дає можливість виконання різних операцій з ним.

Як використовувати?

```Elm
stringLength : String -> Int
stringLength str =
   String.length str
```
Використовуйте вбудовану функцію `String.length`, передаючи їй рядок як аргумент. В результаті буде повернуто кількість символів у цьому рядку. Наприклад, виклик `stringLength "Привіт"` поверне значення `5`, оскільки слово "Привіт" має п'ять літер.

Поглиблене вивчення

Функція `String.length` є стандартною частиною модуля `String` і доступна для використання в будь-яких програмах на Elm. У більш старих версіях мови, для знаходження довжини рядка використовувалась функція `List.length`, яка приймала список символів. Однак, у зв'язку з тим, що в новіших версіях Elm рядки були оптимізовані як окремий тип даних, функція `String.length` була створена для покращення продуктивності.

Повідомлення

Більше інформації про рядки та їх використання можна знайти у документації зі стандартної бібліотеки Elm: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)

Див. також

-  [https://dev.to/kwame/opera-landing-pages-with-elm-538a](https://dev.to/kwame/opera-landing-pages-with-elm-538a) - стаття про використання Elm у реальному проекті для створення посадової сторінки.
- [https://medium.com/@borisyankov/why-learn-elm-as-a-junior-6e54a2c264b2](https://medium.com/@borisyankov/why-learn-elm-as-a-junior-6e54a2c264b2) - стаття про те, чому варто вивчати Elm.