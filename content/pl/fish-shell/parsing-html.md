---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:31:18.733618-07:00
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsing HTML to przekształcanie kodu HTML na strukturalne informacje, które program może łatwo zrozumieć i manipulować. Programiści robią to, by automatyzować interakcje z witrynami, ekstrahować dane lub testować aplikacje webowe.

## How to: (Jak to zrobić:)
Fish nie ma wbudowanych funkcji do parsowania HTML, co oznacza, że będziemy używać narzędzi zewnętrznych, jak `pup` (jeśli jeszcze tego nie masz, najpierw zainstaluj go używając `brew install pup` na macOS lub odpowiedniego polecenia dla Twojego systemu).

```Fish Shell
# Przykład pobierania tytułów z dokumentu HTML za pomocą 'pup'
curl -s https://example.com | pup 'h1 text{}'
```
Wyniki będą wylistowane jeden pod drugim w terminalu.

## Deep Dive (Dogłębna analiza)
Parsowanie HTML stało się rzeczą powszechną wraz z rosnącą popularnością internetu. W przeszłości programiści często stosowali własne, często zawodne, metody ekstrakcji danych. Dziś mamy narzędzia dedykowane tej czynności, jak `pup`, `beautifulsoup` w Pythonie czy `cheerio` w Node.js.

Alternatywy dla `pup`:
- `xmllint` - jeśli potrzebujesz czegoś lżejszego;
- `tidy` - jeśli musisz również uporządkować HTML.

Wybór narzędzia zależy od złożoności zadania. `pup`, będąc strumieniowym parserem, jest idealny do szybkich, prostych zadań. Dla skomplikowanych zadań, rozważ użycie pełnoprawnych bibliotek języka programowania.

## See Also (Zobacz także)
- Oficjalna strona `pup`: https://github.com/ericchiang/pup
- Dokumentacja `beautifulsoup`: https://www.crummy.com/software/BeautifulSoup/
- Dokumentacja `cheerio`: https://cheerio.js.org/

Korzystając z tych źródeł, możesz zagłębić się w tematykę i poszerzyć swoje umiejętności w parsowaniu HTML.
