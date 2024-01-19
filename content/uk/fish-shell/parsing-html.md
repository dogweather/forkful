---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Що та чому?

- Парсинг HTML - це процес видобування конкретної інформації з HTML коду.
- Розробники цим займаються, щоб автоматизувати задачі обробки веб-вмісту, як основу веб-скрапінгу, або для роботи з веб-сторінками на низькому рівні.

## Як це зробити:

```Fish Shell
function html_parse --description 'Parse HTML tag'
  echo $argv | string match -r $argv"<(.|\n)*?>"
end
```

Приклад використання:
```Fish Shell
html_parse '<div>Hello Ukraine!</div>'
# Output: Hello Ukraine!
```

## Поглиблено: 

- Парсинг HTML існує з початку вебу, тому що це основний інструмент для взаємодії з HTML сторінками.
- Є багато альтернативних методів парсингу HTML в інших оболонках та мовах програмування, наприклад, Python, Bash, Perl.
- Використання regex для парсингу HTML є одним з найпростіших методів, але він має свої недоліки - реєстр невдовзі стає дуже складним для справді складних випадків обробки HTML.

## Дивіться також:

[HTML parsing in Fish](https://fishshell.com/docs/current/commands.html#string)
[HTML Parsing in other languages](https://en.wikipedia.org/wiki/Web_scraping#Techniques)