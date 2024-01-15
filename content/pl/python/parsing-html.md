---
title:                "Analiza składni HTML"
html_title:           "Python: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, któremu zależy na wykorzystaniu informacji na stronach internetowych, to parsowanie HTML może być przydatną umiejętnością. Pozwoli Ci ono na wyodrębnianie potrzebnych danych z różnych witryn internetowych.

## Jak to zrobić

```Python
from bs4 import BeautifulSoup
import requests

# pobranie strony internetowej
r = requests.get("https://example.com")
# przekazanie zawartości do biblioteki  BeautifulSoup
soup = BeautifulSoup(r.content, "html.parser")
# wyszukanie elementu <h1> i wyświetlenie jego zawartości
print(soup.find("h1").text)
```

Wyjście:
```
Przykładowy tytuł strony
```

## Głębsze wgląd

Parsowanie HTML polega na wyodrębnieniu informacji z kodu źródłowego strony internetowej. Jest to szczególnie przydatne, gdy chcemy pozyskać dane, takie jak ceny produktów, osiągnięcia sportowe czy aktualności z różnych witryn. Biblioteka BeautifulSoup jest jednym z najpopularniejszych narzędzi do parsowania HTML w Pythonie, oferującym wygodne i proste metody do wyciągania potrzebnych informacji.

## Zobacz także

- [Dokumentacja biblioteki BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Poradnik do nauki biblioteki BeautifulSoup (język angielski)](https://www.dataquest.io/blog/web-scraping-tutorial-python/)
- [Inne przydatne narzędzia do parsowania HTML w Pythonie](https://realpython.com/beautiful-soup-web-scraper-python/)