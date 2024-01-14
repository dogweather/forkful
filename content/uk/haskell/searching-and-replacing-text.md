---
title:    "Haskell: Пошук та заміна тексту"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Чому

Редагування тексту є необхідною частиною програмування, і пошук і заміна тексту є одним з найпоширеніших завдань програмістів. Незалежно від того, чи ви розділяєте текстовий файл, створюєте базу даних або просто форматуєте вихідні дані, вам скоріш за все потрібно буде знайти і замінити певні фрагменти тексту. Завдяки Haskell, це завдання може бути зроблено швидко та ефективно.

## Як це зробити

Щоб здійснити пошук і заміну тексту в Haskell, ми використаємо вбудовану функцію `substitute` з пакету `Text.Regex`. Ця функція приймає шаблон для пошуку, шаблон для заміни та вхідний рядок. Ось приклад, як це можна зробити:

```Haskell
import Text.Regex

strToReplace = "Hello, world!"
regexp = mkRegex "world"
substitute regexp "Haskell" strToReplace
```

Результатом цього буде рядок `"Hello, Haskell!"`, оскільки ми замінили шаблон `"world"` на `"Haskell"` у вхідному рядку.

## Глибше в деталі

Крім вбудованої функції `substitute`, пакет `Text.Regex` також містить багато інших корисних функцій для роботи з регулярними виразами. Наприклад, функція `matchRegex` використовується для пошуку відповідності шаблону в заданому рядку. Це корисно, якщо ви хочете перевірити, чи відповідає рядок певним критеріям.

Для більш складних використань, таких як пошук та заміна з використанням змінних шаблонів, необхідно використовувати складніші функції, такі як `subRegex` та `subRegexWithReplace`. Більше інформації про ці функції можна знайти у [документації](https://hackage.haskell.org/package/regex-base) пакету `regex-base`.

## Дивіться також

- [Офіційна документація Haskell](https://www.haskell.org/documentation/)
- [Навчальний курс з програмування на Haskell](https://stepik.org/course/75)
- [Курси та матеріали для навчання Haskell](https://wiki.haskell.org/Haskell_in_education)