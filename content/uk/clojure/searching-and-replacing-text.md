---
title:                "Clojure: Пошук та заміна тексту"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні, знайти і замінити текст є необхідною частиною для вирішення багатьох завдань, таких як форматування даних, виправлення помилок або заміна застарілих кодів. Використання Clojure для пошуку і заміни тексту є простим і ефективним шляхом, який може зекономити час та зусилля програмістів.

## Як

Для пошуку і заміни тексту в Clojure використовується функція `replace`. Вона приймає два аргументи: зразок для пошуку і зразок для заміни.

```Clojure
(replace "cat" "dog" "I have a cat")
```

Цей код поверне рядок "I have a dog". У разі, якщо зразок для пошуку зустрічається декілька разів, всі входження будуть замінені.

```Clojure
(replace "red" "blue" "The sky is red and the flowers are red")
```

Результатом буде "The sky is blue and the flowers are blue".

## Детальний аналіз

Крім того, Clojure надає можливість використовувати регулярні вирази для пошуку і заміни тексту. Наприклад:

```Clojure
(replace #"w(al\w+)" "good $1" "I want to walk")
```

Результатом буде "I want to good walk". У цьому прикладі, ми використовуємо зразок `"w(al\w+)"`, щоб знайти слова, які починаються з "wal" і містять ще одну літеру після цього. Згодом, за допомогою `$1` ми замінюємо відповідне знаходження нашого зразка.

Ще однією корисною функцією є `replace-first`. Ця функція шукає перше входження зразка, а потім замінює його. Наприклад:

```Clojure
(replace-first "red" "blue" "The sky is red and the flowers are red")
```

Результатом буде "The sky is blue and the flowers are red". Також є можливість використовувати регулярні вирази з `replace-first`.

## Дивись також

- [Clojure документація по функції `replace`](https://clojuredocs.org/clojure.core/replace)
- [Уроки Clojure на сайті Clojure.org](https://clojure.org/guides/learn/symbol)