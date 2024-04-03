---
date: 2024-01-20 17:57:44.072198-07:00
description: "How to (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438) ."
lastmod: '2024-03-13T22:44:50.039854-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

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
