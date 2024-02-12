---
title:                "Інтерполяція рядків"
aliases: - /uk/fish-shell/interpolating-a-string.md
date:                  2024-01-20T17:51:14.954703-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що та Навіщо?

Інтерполяція рядків — це вставка змінних або виразів у рядок. Програмісти це роблять, щоб легко формувати динамічний контент.

## How to:
## Як це зробити:

```Fish Shell
set name "Василь"
echo "Привіт, $name!"
```
Вивід: `Привіт, Василь!`

```Fish Shell
set count 5
echo "У вас залишилось $count спроб."
```
Вивід: `У вас залишилось 5 спроб.`

```Fish Shell
set item "книга"
set price 200
echo "Ціна за одну $item: $price гривень."
```
Вивід: `Ціна за одну книга: 200 гривень.`

## Deep Dive
## Поглиблений Розгляд

Ще з баш-подібних шелів ми інтерполюємо рядки для зручності. У Fish, відмінною особливістю є відсутність необхідності використовувати скобки для інтерполяції, на відміну від `bash`. Є альтернативи, як от форматування з printf або використання з'єднання рядків за допомогою команди `string join`. За лаштунками, Fish обробляє вставку змінної у рядок безпосередньо, забезпечуючи чистоту синтаксису та простоту використання.

## See Also
## Дивіться Також

- Офіційна документація Fish Shell: [Interpolation](https://fishshell.com/docs/current/index.html#syntax-command-substitutions)
- Корисні приклади інтерполяції рядків у Fish: [Fish Shell Cookbook](https://github.com/jorgebucaran/fish-shell-cookbook)
- Різниця між Fish і іншими шелами: [Fish vs Bash](https://fishshell.com/docs/current/tutorial.html#tut_why_fish)
