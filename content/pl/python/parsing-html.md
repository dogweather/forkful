---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:33:27.853083-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsing HTML to proces wydobywania danych ze struktur HTML. Programiści to robią, by manipulować zawartością stron internetowych lub scrapować informacje.

## Jak to zrobić:
Użyjemy biblioteki `BeautifulSoup`, aby zeskrapować tytuł strony.

```Python
from bs4 import BeautifulSoup
import requests

url = "http://example.com"
response = requests.get(url)
soup = BeautifulSoup(response.text, 'html.parser')

title = soup.find('title').get_text()
print(f"Tytuł strony to: '{title}'")
```

Wynik:
```
Tytuł strony to: 'Example Domain'
```

## Głębsze spojrzenie:
Parsing HTML nie jest nowym tematem. W latach '90, kiedy internet zaczął się rozwijać, potrzeba analizowania HTML-a rosła. W Pythonie, poza `BeautifulSoup`, mamy też `lxml` i wbudowany moduł `html.parser`. Każda z nich ma swoje plusy i minusy: `lxml` jest szybszy, ale wymaga dodatkowych zależności C, a `html.parser` jest częścią standardowej biblioteki Pythona, ale jest wolniejszy i mniej elastyczny.

## Zobacz również:
- Dokumentacja `BeautifulSoup`: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Porównanie parserów HTML w Pythonie: https://www.crummy.com/software/BeautifulSoup/bs4/doc/#installing-a-parser
- Tutorial Web Scraping w Pythonie: https://realpython.com/beautiful-soup-web-scraper-python/
