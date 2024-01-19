---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Pobieranie Strony Internetowej w Pythonie

## Co i Dlaczego?
Pobieranie strony internetowej to proces zapisywania jej zawartości na dysku twardym. Programiści robią to, żeby analizować dane, skrapować informacje lub monitorować zmiany na stronie.

## Jak to zrobić:
Python dostarcza wiele narzędzi do tej pracy, ale najbardziej popularnym jest biblioteka `requests`. Poniżej znajduje się prosta metoda do pobrania strony:

```Python
import requests

url = 'http://example.com'
response = requests.get(url)

# Pobrane dane są dostępne za pomocą metody .text
print(response.text)
```
Po uruchomieniu tego kodu, na ekranie powinien pojawić się kod HTML pobranej strony.

## Głębsze Zanurzenie
**Historia**: Na początku istnienia internetu, aby pobrać stronę, trzeba było korzystać z niskopoziomowych bibliotek sieciowych. Dzisiaj mamy do dyspozycji wiele wysokopoziomowych narzędzi, takich jak `requests`.

**Alternatywy**: Inne biblioteki, które oferują podobne funkcje to `urllib` i `http.client`. Są one wbudowane w Pythona i nie wymagają dodatkowej instalacji, ale są trudniejsze w obsłudze.

**Szczegóły Implementacji**: Kiedy robisz zapytanie GET za pomocą `requests.get()`, wywołujesz kilka warstw abstrakcji obsługujących połączenie sieciowe, przekierowania, kodowanie znaków i wiele innych.

## Zobacz Także
1. Oficjalna dokumentacja `requests`: https://requests.readthedocs.io/en/master/
2. Dokumentacja `urllib`: https://docs.python.org/3/library/urllib.html
3. Dokumentacja `http.client`: https://docs.python.org/3/library/http.client.html
4. Przewodnik `BeautifulSoup` do analizy HTML: https://www.crummy.com/software/BeautifulSoup/bs4/doc/