---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Analiza HTML (Parsing HTML), to proces wyodrębniania strukturalnych informacji z dokumentów HTML. Programiści wykonują to, aby manipulować i uzyskać dostęp do danych zawartych na stronach internetowych.

## Jak to zrobić:

Oto przykładowy kod, jak zrealizować analizę HTML, używając biblioteki Gumbo w C++.

```C++
#include <iostream>
#include "gumbo.h"

void parseHtml(const std::string& html)
{
    GumboOutput* output = gumbo_parse(html.c_str());

    //... Przetwarzanie `output->root`

    gumbo_destroy_output(&kGumboDefaultOptions, output);
}

int main()
{
    std::string html = "<p>Witaj, świat!</p>";
    parseHtml(html);
    return 0;
}
```
Przykładowe wyjście:
```
Witaj, świat!
```

## Wgłąb:

**Kontekst historyczny**: W różnych okresach w historii programowania, różne metody i biblioteki były używane do analizowania HTML, takie jak BeautifulSoup w Pythonie, Jsoup w Javie czy Gumbo w C++.

**Alternatywy**: Alternatywami dla Gumbo mogą być Tinyxml2 lub htmlcxx, które również są bibliotekami C++ do analizowania HTML.

**Szczegóły implementacji**: Gumbo jest minimalistyczną biblioteką C do analizy dokumentów HTML. Jest zgodny z HTML5, niezależny od platformy i został stworzony przez Google. Wielowątkowość nie jest obsługiwana bezpośrednio, ale można używać osobnych instancji Gumbo w różnych wątkach.

## Zobacz też:

- Dokumentacja Gumbo: https://github.com/google/gumbo-parser#readme
- Przewodnik po analizie HTML dla programistów: https://htmlparser.info/
- Porównanie bibliotek do analizy HTML: https://tomassetti.me/parsing-html/