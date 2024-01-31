---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:30:01.443421-07:00
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsing HTML, czyli analiza kodu HTML, pozwala programistom wydobyć dane z dokumentów HTML. Robimy to, by przetworzyć strukturyzowaną zawartość internetową na użyteczne informacje, czy to w celu indeksowania, scrapingu danych czy innych operacji.

## How to: (Jak to zrobić:)
Bash nie jest idealny do parsowania HTML, ale w razie potrzeby możesz użyć narzędzi jak `grep`, `sed`, `awk`, a najlepiej `xmllint` czy `pup`.

```Bash
# Pobierz tytuł strony za pomocą xmllint
curl -s http://example.com | xmllint --html --xpath '//title/text()' 2>/dev/null

# Użycie pup do wydobycia linków
curl -s http://example.com | pup 'a attr{href}'
```

Wyjście może wyglądać następująco:
```
Strona Przykładowa
http://example.com/link1
http://example.com/link2
```

## Deep Dive (Dogłębna analiza)
Historia parsowania HTML jest tak stara jak i samo HTML. Początkowo polegała na prostych skryptach i narzędziach typu `grep`. Obecnie, lepsze do tego są specjalnie zaprojektowane języki i biblioteki, jak Beautiful Soup dla Pythona czy Nokogiri dla Ruby.

Alternatywy dla Bash-a to wspomniane już Python, Ruby, czy nawet Node.js z różnorodnymi i efektywnymi parserami.

Parsowanie HTML w Bashu to przeważnie kombinowanie z wyrażeniami regularnymi i narzędziami tekstowymi. Nie jest to idealne, bo HTML nie jest regularnym językiem i może prowadzić do błędów, ale w prostych przypadkach może się sprawdzić. Ważne jest, aby używać narzędzi, które respektują strukturę XML, jak `xmllint`.

## See Also (Zobacz również)
- `[Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)` - dokumentacja do parsera dla Pythona.
- `[Nokogiri Website](https://nokogiri.org/)` - strona główna parsera dla Ruby.
- `[pup GitHub Repo](https://github.com/EricChiang/pup)` - projekt pup na GitHubie, dla narzędzia działającego z linii komend.
- `[xmllint Manual](http://xmlsoft.org/xmllint.html)` - manual do xmllint, narzędzia do XML i HTML.
