---
title:                "Analiza składniowa HTML"
aliases: - /pl/python/parsing-html.md
date:                  2024-02-03T19:12:50.370614-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie HTML-a polega na analizowaniu kodu HTML strony internetowej w celu wyodrębnienia określonych informacji lub elementów, co jest powszechnym zadaniem przy web scrapingu, wydobywaniu danych lub automatyzowaniu interakcji ze stronami internetowymi. Programiści wykonują to, aby programistycznie wchodzić w interakcje z witrynami internetowymi, automatyzować zadania lub testować aplikacje internetowe.

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
