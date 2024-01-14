---
title:    "Haskell: Перетворення рядка на малі літери"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Чому

Конвертування рядка до нижнього регістру є важливою задачею при роботі з текстовими даними в Haskell. Використання нижнього регістру допомагає уникнути проблем з різницею між верхнім і нижнім регістром при порівнянні та пошуку рядків.

## Як

Конвертація рядка до нижнього регістру може бути здійснена за допомогою багатьох методів в Haskell. Найпростішим з них є використання функції `toLower` з модуля `Data.Char`. Наприклад, якщо ми маємо рядок "Привіт Світ!", код буде виглядати так:

```Haskell
import Data.Char (toLower)

lowercaseStr = map toLower "Привіт Світ!"

putStrLn lowercaseStr
-- "привіт світ!"
```

Також можна використовувати функцію `map` в поєднанні з функцією `toLower` для конвертації кожного символу у рядку окремо:

```Haskell
lowercaseStr = map toLower "Привіт Світ!"

putStrLn lowercaseStr
-- "приіт сїт!"
```

Крім цього, є багато інших методів для конвертації рядка до нижнього регістру, наприклад, використання функції `map` з лямбда-виразом або використання розширення `OverloadedStrings`. Для детальнішого огляду цих методів дивіться секцію "Глибоке дослідження".

## Глибоке дослідження

Конвертування рядка до нижнього регістру може бути не таким простим, як здається на перший погляд. Наприклад, при роботі з кириличними символами української абетки, поведінка функцій для конвертації може бути неочікуваною. Також важливо враховувати, які мовні налаштування використовуються на комп'ютері, оскільки це може вплинути на результат конвертації.

Для уникнення проблем з кодуванням символів та мовними налаштуваннями, рекомендується використовувати бібліотеки, які спеціально розроблені для обробки рядків українською мовою, наприклад, `ukrainian-transliterators` або `ukrainian-strings`.

## Дивіться також

- [Hackage - Data.Char module](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Hackage - ukrainian-transliterators](https://hackage.haskell.org/package/ukrainian-transliterators)
- [Hackage - ukrainian-strings](https://hackage.haskell.org/package/ukrainian-strings)