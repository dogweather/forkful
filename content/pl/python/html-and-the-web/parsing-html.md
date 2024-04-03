---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:50.370614-07:00
description: "Jak to zrobi\u0107: Python oferuje pot\u0119\u017Cne biblioteki takie\
  \ jak BeautifulSoup i requests do web scrapingu i parsowania HTML-a. Aby zacz\u0105\
  \u0107, musisz zainstalowa\u0107\u2026"
lastmod: '2024-03-13T22:44:34.947229-06:00'
model: gpt-4-0125-preview
summary: "Python oferuje pot\u0119\u017Cne biblioteki takie jak BeautifulSoup i requests\
  \ do web scrapingu i parsowania HTML-a."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Jak to zrobić:
Python oferuje potężne biblioteki takie jak BeautifulSoup i requests do web scrapingu i parsowania HTML-a. Aby zacząć, musisz zainstalować te biblioteki, jeśli jeszcze tego nie zrobiłeś:

```bash
pip install beautifulsoup4 requests
```

Oto podstawowy przykład użycia `requests` do pobrania zawartości HTML strony internetowej oraz `BeautifulSoup` do jej analizy:

```python
import requests
from bs4 import BeautifulSoup

# Pobierz zawartość strony internetowej
URL = 'https://example.com'
page = requests.get(URL)

# Analizuj zawartość HTML
soup = BeautifulSoup(page.content, 'html.parser')

# Przykład wyodrębnienia tytułu strony internetowej
title = soup.find('title').text
print(f'Tytuł strony internetowej: {title}')
```

**Przykładowy wynik**:
```
Tytuł strony internetowej: Domena przykładowa
```

W przypadku bardziej złożonych zapytań, takich jak wyodrębnienie wszystkich linków ze strony internetowej, możesz użyć różnych metod BeautifulSoup do nawigowania i przeszukiwania drzewa struktury:

```python
# Wyodrębnij wszystkie linki w znacznikach <a>
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**Przykładowy wynik**:
```
https://www.iana.org/domains/example
```

Elastyczność BeautifulSoup pozwala na dostosowanie wyszukiwania do dokładnie potrzebnych danych, co czyni parsowanie HTML potężnym narzędziem dla programistów pracujących z treściami internetowymi.
