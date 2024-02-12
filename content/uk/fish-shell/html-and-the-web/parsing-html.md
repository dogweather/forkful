---
title:                "Аналіз HTML"
aliases:
- /uk/fish-shell/parsing-html.md
date:                  2024-02-03T19:12:28.127421-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Парсинг HTML полягає в екстракції даних або інформації з HTML-контенту, звичайній задачі при роботі з веб-данними. Програмісти роблять це, щоб автоматизувати витягу інформації з вебсайтів для таких завдань, як веб скрапінг, добування даних або автоматизоване тестування.

## Як робити:

Переважно, Fish shell не призначена для безпосереднього парсингу HTML. Проте, вона відмінно справляється з об'єднанням інструментів Unix таких як `curl`, `grep`, `sed`, `awk`, або використанням спеціалізованих інструментів як `pup` чи `beautifulsoup` в Python скрипті. Нижче наведено приклади, що демонструють, як використовувати ці інструменти з Fish shell для парсингу HTML.

### Використання `curl` та `grep`:
Завантаження HTML-контенту та витягування рядків, що містять посилання:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Вивід:
```
/page1.html
/page2.html
...
```

### Використання `pup` (командного інструменту для парсингу HTML):

Спочатку переконайтеся, що `pup` встановлено. Потім ви можете використовувати його для витягу елементів за їх тегами, ідентифікаторами, класами і т.д.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Вивід, схожий на приклад з `grep`, буде перелічувати атрибути href тегів `<a>`.

### З Python скриптом та `beautifulsoup`:

Хоча Fish сам по собі не може парсити HTML нативно, він безпроблемно інтегрується з Python скриптами. Нижче наведено стислий приклад, що використовує Python з `BeautifulSoup` для парсингу та витягу назв з HTML. Переконайтеся, що у вашому Python середовищі встановлені `beautifulsoup4` та `requests`.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Використання:

```fish
parse_html 'https://example.com'
```

Вивід:
```
Example Domain
```

Кожен з цих методів служить різним випадкам використання і масштабам складності, від простої маніпуляції текстом в командному рядку до повного парсинг потужності `beautifulsoup` в Python скриптах. Залежно від ваших потреб і складності структури HTML, можна обрати прямолінійний Unix pipeline або більш потужний скриптовий підхід.
