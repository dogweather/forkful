---
title:                "Pobieranie strony internetowej"
html_title:           "Python: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Cześć! Dzisiaj pokażę Ci, jak pobrać stronę internetową za pomocą Pythona. Jest to przydatna umiejętność, którą każdy programista powinien znać.

## Co i dlaczego?

Pobieranie strony internetowej oznacza pobranie jej treści, takiej jak tekst, obrazy i inne zasoby, do naszego programu lub systemu. Programiści często pobierają strony internetowe, aby analizować dane, wyświetlać je w aplikacji lub tworzyć własne narzędzia internetowe.

## Jak to zrobić:

```Python
import requests

url = 'https://www.nike.com/pl/pl_pl/c/obuwie'
r = requests.get(url)

print(r.text)
```

Output:
```
<!DOCTYPE html>
<html lang="pl"><head>
  <meta charset="UTF-8">
  ...
```

## Głębsza analiza

1. Kontekst historyczny:
Pobieranie stron internetowych było jednym z pierwszych sposobów na zastosowanie Pythona w analizie internetowej. Wcześniej użytkownicy korzystali z bibliotek takich jak urllib czy urllib2, ale teraz preferowanym sposobem jest moduł requests.

2. Alternatywy:
Ponadto istnieją inne biblioteki, takie jak Beautiful Soup czy mechanize, które mogą również być wykorzystywane do pobierania stron internetowych. Jednak requests jest uważany za łatwiejszy w użyciu i bardziej wydajny.

3. Szczegóły implementacji:
Moduł requests jest dostępny jako zewnętrzna biblioteka i musi być zainstalowany za pomocą narzędzia pip. Aby pobrać stronę internetową, najpierw musimy użyć funkcji requests.get() i przekazać jej adres URL. Następnie można uzyskać dostęp do treści strony za pomocą pobieranego obiektu response.

## Zobacz również:

1. Dokumentacja modułu requests: https://requests.readthedocs.io/en/master/
2. Podręcznik Pythona: https://docs.python.org/pl/3/howto/urllib2.html
3. Beautiful Soup: https://www.crummy.com/software/BeautifulSoup/bs4/doc
4. Mechanize: https://pypi.org/project/mechanize/