---
title:                "Використання регулярних виразів"
date:                  2024-01-19
simple_title:         "Використання регулярних виразів"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що і Чому?
Регулярні вирази - це мовні шаблони для збігу тексту. Програмісти використовують їх, щоб швидко знаходити, заміняти або перевіряти певні фрагменти коду та даних.

## Як це робити:
```Fish Shell
# Знайти рядки, що містять слово "fish"
echo "I like fish shell scripting" | string match -r 'fish'

# Результат:
fish

# Замінити всі цифри на зірочки
echo "Call me on 123-456-7890" | string replace -ra '[0-9]' '*'

# Результат:
Call me on ***-***-****

# Витягти слова, які починаються на 's' і закінчуються на 'e'
echo "simple sample script style" | string match -r 's[a-z]*e'

# Результат:
simple
sample
style
```

## Поглиблений Розгляд
Регулярні вирази з'явились у 1950-х, коли математик Стівен Кліні створив формальну теорію. Альтернативи включають шаблони globbing та фіксовані рядкові порівняння, але вони менш гнучкі. У Fish Shell регулярні вирази реалізовані через вбудовану команду `string`, що спрощує маніпуляції з текстом.

## Дивись також:
- Офіційну документацію Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Онлайн-інструмент для тестування регулярних виразів: [https://regexr.com/](https://regexr.com/)
- Детальний туторіал по регулярним виразам: [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)
