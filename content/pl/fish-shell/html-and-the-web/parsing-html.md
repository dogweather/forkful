---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:15.896313-07:00
description: "Parsowanie HTML polega na wydobywaniu danych lub informacji z tre\u015B\
  ci HTML, co jest powszechnym zadaniem podczas pracy z danymi internetowymi. Programi\u015B\
  ci\u2026"
lastmod: '2024-03-13T22:44:35.838978-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie HTML polega na wydobywaniu danych lub informacji z tre\u015B\
  ci HTML, co jest powszechnym zadaniem podczas pracy z danymi internetowymi."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Jak to zrobić:
Shell Fish, przede wszystkim, nie jest zaprojektowany do bezpośredniego parsowania HTML. Jednak świetnie sprawdza się w łączeniu narzędzi Unix, takich jak `curl`, `grep`, `sed`, `awk`, lub w wykorzystaniu specjalistycznych narzędzi takich jak `pup` czy `beautifulsoup` w skrypcie Python. Poniżej znajdują się przykłady, które pokazują, jak wykorzystać te narzędzia w obrębie shella Fish do parsowania HTML.

### Wykorzystanie `curl` i `grep`:
Pobieranie treści HTML i ekstrahowanie linii zawierających linki:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Wyjście:
```
/page1.html
/page2.html
...
```

### Użycie `pup` (narzędzia wiersza poleceń do parsowania HTML):
Najpierw upewnij się, że `pup` jest zainstalowany. Następnie możesz go użyć do ekstrahowania elementów po ich tagach, identyfikatorach, klasach itp.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Wyjście, podobnie jak w przypadku przykładu z `grep`, wylistuje atrybuty href tagów `<a>`.

### Z skryptem Pythona i `beautifulsoup`:
Mimo że Fish sam w sobie nie może parsować HTML natywnie, bezproblemowo integruje się ze skryptami Pythona. Poniżej znajduje się zwięzły przykład wykorzystania Pythona z `BeautifulSoup` do parsowania i ekstrakcji tytułów z HTML. Upewnij się, że masz zainstalowane `beautifulsoup4` oraz `requests` w swoim środowisku Pythona.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

odpowiedz = requests.get(sys.argv[1])
zupa = BeautifulSoup(odpowiedz.text, 'html.parser')

tytuly = zupa.find_all('title')

for title in tytuly:
    print(title.get_text())
" $url
end
```

Użycie:

```fish
parse_html 'https://example.com'
```

Wyjście:
```
Przykładowa Domena
```

Każda z tych metod służy różnym przypadkom użycia i skalom złożoności, od prostych manipulacji tekstem w wierszu poleceń po pełną moc parsowania `beautifulsoup` w skryptach Pythona. W zależności od twoich potrzeb i złożoności struktury HTML, możesz wybrać prosty pipeline Unix lub potężniejsze podejście skryptowe.
