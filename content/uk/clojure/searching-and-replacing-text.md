---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?

Пошук та заміна тексту - це просто засіб знайдення певного виразу (або «рядка») в тексті та заміни його на інший. Програмісти роблять це, щоб редагувати, оновлювати або виправляти інформацію в коді.

## Як виконати:

Clojure має декілька функцій для пошуку та заміни тексту. Одним з них є функція `clojure.string/replace`. Давайте подивимося на невеликий приклад:

```Clojure
(require '[clojure.string :as str])

(defn replace-text
  [text find replace-with]
  (str/replace text find replace-with))
  
(replace-text "Hello, World!" "World" "Ukraine") ; Виведе "Hello, Ukraine!"
```
В цьому прикладі, ми імпортуємо простір імен `clojure.string` як `str`. Ми визначаємо функцію, `replace-text`, яка приймає три аргументи: `text` (текст для пошуку та заміни), `find` (рядок, який потрібно знайти) і `replace-with` (рядок, на який потрібно замінити `find`). Функція повертає текст з заміненими значеннями.

## Занурення в деталі:

Пошук і заміна тексту - це старий як світ комп'ютерний підхід, який існує ще з часів народження програмування. Є багато альтернатив `clojure.string/replace` в Clojure, зокрема `clojure.string/replace-first`, який замінює лише перше співпадіння, або регулярні вирази для більш складних задач заміни.

Коли ви викликаєте `clojure.string/replace`, він працює наївно: проходить по всьому рядку зліва направо, замінюючи кожне співпадіння, яке знаходить. Це проста та ефективна імплементація, що покриває більшість випадків використання.

## Також бачити:

- Документація Clojure: [clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)