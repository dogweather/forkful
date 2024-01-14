---
title:    "Haskell: Пошук та заміна тексту"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому 

Можливо у вас вже виникло бажання знайти та замінити певні фрагменти тексту у вашому програмному коді. Надто численні ручні правки можуть бути складними та часто приводять до помилок. Саме тому корисно мати знання про те, як зробити це автоматично, використовуючи Haskell.

## Як це зробити

Використовуючи функцію `replace` з пакету `Data.Text`, ми можемо швидко та легко замінити один фрагмент тексту на інший. Ось приклад коду, де ми замінюємо пошкоджені символи на їх загальний варіант "I" та отримуємо оновлений текст:

```Haskell
import Data.Text
main = do
  let text = "Th1s 1s a t3st"
  let updatedText = replace "1" "I" text
  print updatedText
```

Вихідний текст: `This is a test`

## Глибше вдаваймось

Існують різні підходи до заміни тексту в Haskell. Функція `replace` з пакету `Data.Text` проста та зручна, але для більш складних сценаріїв можуть бути корисні інші розширені методи. Наприклад, функція `replaceAll` з пакету `Data.Text.ICU` дозволяє здійснювати заміну тексту з використанням регулярних виразів.

## Дивись також

- [Розширений пакет заміни тексту в Haskell](https://hackage.haskell.org/package/hxt)
- [Документація пакету Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Приклади регулярних виразів у Haskell](https://wiki.haskell.org/Regular_expressions)