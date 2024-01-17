---
title:                "Pobieranie strony internetowej"
html_title:           "Bash: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej to proces pobierania zawartości strony internetowej z internetu i zapisywania jej na lokalnym komputerze. Programiści często pobierają strony internetowe, aby przetwarzać lub analizować ich zawartość w swoim kodzie.

## Jak to zrobić:

```Bash
wget www.example.com
```

Kod powyżej pokaże przykładowe użycie polecenia `wget` w celu pobrania strony internetowej o nazwie domeny "example.com". Po wykonaniu tego polecenia, zawartość strony zostanie zapisana jako plik o nazwie "index.html" w bieżącym katalogu.

Aby pobrać inną stronę internetową, wystarczy zmienić adres URL po słowie kluczowym `wget` w kodzie.

## Głębsza analiza:

Pierwotnie, przed narzędziami, które mamy obecnie, programiści musieli ręcznie pobierać zawartość strony internetowej. Teraz, dzięki narzędziom, takim jak `wget`, proces ten jest zautomatyzowany i znacznie prostszy.

Alternatywnym sposobem pobierania stron internetowych jest użycie narzędzi do wydobywania danych, takich jak Beautiful Soup czy lxml. Jednak, jeśli potrzebujesz pobierać wiele stron, narzędzie do pobierania, takie jak `wget`, jest szybszym i bardziej wydajnym rozwiązaniem.

Implementacja pobierania stron internetowych jest możliwa dzięki protokołowi HTTP / HTTPS, który umożliwia pobieranie danych z serwera internetowego. `wget` korzysta z tego protokołu, aby wysłać zapytanie do serwera i pobrać odpowiedź zawierającą kod HTML strony.

## Zobacz również:

- [Dokumentacja `wget`](https://www.gnu.org/software/wget/manual/wget.html)
- [Beautiful Soup - narzędzie do analizowania danych na stronach internetowych](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [lxml - biblioteka Python umożliwiająca analizę danych z plików XML i HTML](https://lxml.de/)