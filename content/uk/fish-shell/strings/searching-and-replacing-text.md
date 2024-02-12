---
title:                "Пошук та заміна тексту"
aliases: - /uk/fish-shell/searching-and-replacing-text.md
date:                  2024-01-20T17:57:44.072198-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Пошук та заміна тексту – це про акт знаходження певного шматочка тексту та його заміни на інший. Програмісти це роблять, щоб виправити помилки, оновити дані, або автоматизувати редагування.

## How to (Як це зробити)
```Fish Shell
# знайти і замінити 'apple' на 'orange' у файлі fruit.txt
sed 's/apple/orange/g' fruit.txt > updated_fruit.txt

# вивести результати на екран
cat updated_fruit.txt
```
Вивід:
```plaintext
orange
banana
orange
```

## Deep Dive (Поглиблене вивчення)
Спочатку команда `sed` у UNIX (Stream Editor) була створена для фільтрації та перетворення тексту у скриптах шела. Fish Shell, розроблена як більш модерна альтернатива, успадковує ці можливості зі своїх UNIX-подібних попередників.

Fish використовує `sed` для пошуку та заміни, але вносить власні функції для спрощення та зручності, наприклад, управління кольорами виведення.

Існують альтернативи, такі як `awk` для складніших маніпуляцій текстом і `perl` для більш потужних текстових операцій.

## See Also (Дивіться також)
- Офіційна документація Fish Shell: [fishshell.com/docs](https://fishshell.com/docs/current/index.html)
- Сторінка man для `sed`: у терміналі виконайте `man sed`
- GNU `awk` Manual: [GNU awk user's guide](https://www.gnu.org/software/gawk/manual/gawk.html)
