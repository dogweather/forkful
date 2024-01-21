---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:57:48.189228-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що це та Навіщо?
Шукати та замінювати текст — це основна задача у програмуванні, яка полягає у виявленні підрядків і їхній заміні на інші. Програмісти використовують цю операцію для корекції, форматування даних чи автоматизації редагування коду.

## Як це робити:
У Gleam, схожі операции можна виконати за допомогою стандартних бібліотек. Ось простий приклад:

```gleam
import gleam/string

fn replace_example() {
  let text = "The quick brown Fox jumps over the lazy Dog"
  string.replace(text, "Fox", "Cat")
}

pub fn main() {
  replace_example()
}
```

Якщо ви виконаєте цей код, вивід буде наступний:

```
"The quick brown Cat jumps over the lazy Dog"
```

## Поглиблений Розбір
Історично пошук та заміна тексту використовувались у редакторах текстів від самого початку інформатики. Важливими інструментами були команди в редакторах, як-от `sed` в Unix.

Крім стандартного пошуку та заміни, існують регулярні вирази, котрі дозволяють визначати складніші шаблони для пошуку та динамічної заміни текстів.

Gleam використовує строгу типізацію та шаблонну відповідність для роботи зі строками, що гарантує безпеку типів під час виконання операций пошуку та заміни.

## Дивіться Також
- Regular Expressions guide in programming: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Learn more about `sed` and its capacities: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)