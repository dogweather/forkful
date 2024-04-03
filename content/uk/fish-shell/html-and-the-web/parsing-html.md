---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:28.127421-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u041F\u0435\u0440\
  \u0435\u0432\u0430\u0436\u043D\u043E, Fish shell \u043D\u0435 \u043F\u0440\u0438\
  \u0437\u043D\u0430\u0447\u0435\u043D\u0430 \u0434\u043B\u044F \u0431\u0435\u0437\
  \u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\u044C\u043E\u0433\u043E \u043F\u0430\
  \u0440\u0441\u0438\u043D\u0433\u0443 HTML. \u041F\u0440\u043E\u0442\u0435, \u0432\
  \u043E\u043D\u0430 \u0432\u0456\u0434\u043C\u0456\u043D\u043D\u043E \u0441\u043F\
  \u0440\u0430\u0432\u043B\u044F\u0454\u0442\u044C\u0441\u044F \u0437 \u043E\u0431\
  '\u0454\u0434\u043D\u0430\u043D\u043D\u044F\u043C \u0456\u043D\u0441\u0442\u0440\
  \u0443\u043C\u0435\u043D\u0442\u0456\u0432 Unix \u0442\u0430\u043A\u0438\u0445\u2026"
lastmod: '2024-03-13T22:44:50.064880-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0432\u0430\u0436\u043D\u043E, Fish shell \u043D\
  \u0435 \u043F\u0440\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u0430 \u0434\u043B\
  \u044F \u0431\u0435\u0437\u043F\u043E\u0441\u0435\u0440\u0435\u0434\u043D\u044C\u043E\
  \u0433\u043E \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 HTML."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

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
