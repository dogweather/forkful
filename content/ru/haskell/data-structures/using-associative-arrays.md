---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:31.480304-07:00
description: "\u0410\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\
  \u0435 \u043C\u0430\u0441\u0441\u0438\u0432\u044B \u0438\u043B\u0438 \u0441\u043B\
  \u043E\u0432\u0430\u0440\u0438 \u0432 Haskell \u2014 \u044D\u0442\u043E \u0432\u0441\
  \u0435 \u043E \u0442\u043E\u043C, \u0447\u0442\u043E\u0431\u044B \u0441\u043E\u043F\
  \u043E\u0441\u0442\u0430\u0432\u0438\u0442\u044C \u043A\u043B\u044E\u0447\u0438\
  \ \u0441\u043E \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F\u043C\u0438 \u0434\
  \u043B\u044F \u0431\u044B\u0441\u0442\u0440\u043E\u0433\u043E \u043F\u043E\u0438\
  \u0441\u043A\u0430 \u0438 \u044D\u0444\u0444\u0435\u043A\u0442\u0438\u0432\u043D\
  \u043E\u0433\u043E \u0443\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u0438\u044F\
  \ \u0434\u0430\u043D\u043D\u044B\u043C\u0438.\u2026"
lastmod: '2024-03-13T22:44:45.120574-06:00'
model: gpt-4-0125-preview
summary: "\u0410\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\
  \u0435 \u043C\u0430\u0441\u0441\u0438\u0432\u044B \u0438\u043B\u0438 \u0441\u043B\
  \u043E\u0432\u0430\u0440\u0438 \u0432 Haskell \u2014 \u044D\u0442\u043E \u0432\u0441\
  \u0435 \u043E \u0442\u043E\u043C, \u0447\u0442\u043E\u0431\u044B \u0441\u043E\u043F\
  \u043E\u0441\u0442\u0430\u0432\u0438\u0442\u044C \u043A\u043B\u044E\u0447\u0438\
  \ \u0441\u043E \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F\u043C\u0438 \u0434\
  \u043B\u044F \u0431\u044B\u0441\u0442\u0440\u043E\u0433\u043E \u043F\u043E\u0438\
  \u0441\u043A\u0430 \u0438 \u044D\u0444\u0444\u0435\u043A\u0442\u0438\u0432\u043D\
  \u043E\u0433\u043E \u0443\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u0438\u044F\
  \ \u0434\u0430\u043D\u043D\u044B\u043C\u0438.\u2026"
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0430\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\u0445\
  \ \u043C\u0430\u0441\u0441\u0438\u0432\u043E\u0432"
---

{{< edit_this_page >}}

## Что и Почему?

Ассоциативные массивы или словари в Haskell — это все о том, чтобы сопоставить ключи со значениями для быстрого поиска и эффективного управления данными. Программисты используют их для управления коллекциями парных элементов, где поиск элемента — это дело техники, в сравнении со списками.

## Как использовать:

Haskell изначально не имеет ассоциативных массивов так, как это есть в некоторых других языках, но он предлагает мощную стандартную библиотеку под названием `Data.Map` для работы с парами ключ-значение. Давайте закатаем рукава и посмотрим, как их использовать!

Во-первых, убедитесь, что вы импортировали ее:
```Haskell
import qualified Data.Map as Map
```

Создать карту просто. Создадим одну с некоторыми языками программирования и их парадигмами:
```Haskell
let languages = Map.fromList [("Haskell", "Функциональный"), ("Python", "Императивный"), ("Prolog", "Логический")]
```

А теперь, как насчет того, чтобы узнать парадигму Haskell?
```Haskell
Map.lookup "Haskell" languages
-- вывод: Just "Функциональный"
```

Добавление нового языка просто:
```Haskell
let languagesUpdated = Map.insert "Rust" "Системный" languages
```

А если мы хотим перечислить все языки? Используйте `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- вывод: ["Haskell", "Python", "Prolog", "Rust"]
```

Чтобы перечислить парадигмы, используйте `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- вывод: ["Функциональный", "Императивный", "Логический", "Системный"]
```

Эти базовые операции должны покрыть большинство потребностей, но в `Data.Map` есть еще много всего интересного!

## Глубокое погружение

Модуль `Data.Map` в стандартной библиотеке Haskell построен на основе сбалансированных двоичных деревьев, конкретно на AVL деревьях. Этот выбор гарантирует, что большинство операций с картой, такие как вставка, удаление и поиск, могут быть выполнены за время O(log n), где n — количество элементов в карте. Это эффективный выбор для многих случаев использования, хотя и не самый быстрый для всех сценариев.

Есть и историческая нюансика: до того как `Data.Map` стало идти-решением, программисты Haskell часто использовали списки пар для имитации ассоциативных массивов. Однако операции на таких структурах имеют сложность O(n) для поиска, что делает `Data.Map` значительным улучшением с точки зрения производительности.

Теперь, несмотря на эффективность и полезность `Data.Map`, это не всегда лучший инструмент для каждой задачи. Для высокопроизводительных задач, где даже время поиска O(log n) слишком медленное, или когда ключи всегда являются целочисленными значениями, массивы или хеш-таблицы (через `Data.HashMap`) могут предложить лучшую производительность с временем доступа O(1).

Экосистема Haskell позволяет использовать разнообразие структур данных для разных нужд, и `Data.Map` является отличным выбором общего назначения для ассоциативных массивов, сочетая в себе простоту использования, гибкость и производительность.
