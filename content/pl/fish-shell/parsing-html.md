---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Analiza składniowa HTML polega na przetwarzaniu kodu HTML i tworzeniu z niego struktury danych. Programiści robią to, aby zrozumieć i manipulować strukturą dokumentu HTML.

## Jak to zrobić:

Fish shell nie posiada natywnych narzędzi do analizy HTML, ale jest parę zewnętrznych jak [Pup](https://github.com/ericchiang/pup) czy [hxselect](https://www.w3.org/Tools/HTML-XML-utils/README).

Przykład z użyciem Pup:

```Fish Shell
# Pobranie strony i przeparsowanie jej:
curl "https://przykladowa-strona.pl" | pup 'body text{}'
```

Przykład z użyciem hxselect:

```Fish Shell
# Pobranie strony i przeparsowanie jej:
curl "https://przykladowa-strona.pl" | hxnormalize -x | hxselect -i 'body'
```

## Zagłębienie się:

Fish Shell, choć nie posiada wbudowanych narzędzi do analizy składniowej HTML, poprzez zapewnienie łatwej integracji z innymi narzędziami, staje się uniwersalnym skryptem powłoki. Wielu programistów korzysta z Pup lub hxselect, aby przeprowadzić analizę składniową HTML w Fish Shell. Te narzędzia, mimo że nie są tak stare jak HTML, są znacznie młodsze i nadal aktywnie rozwijane.

Poza Pup i hxselect, programiści mają do wyboru wiele alternatyw, takich jak Beautiful Soup dla Pythona czy Nokogiri dla Ruby.

Sposób działania tych narzędzi polega na przetwarzaniu surowego kodu HTML i przekształcaniu go w strukturę drzewiastą, nazywaną Document Object Model (DOM). Następnie, przy pomocy wyrażeń CSS lub XPath, możemy manipulować i uzyskiwać konkretne elementy z tej struktury.

## Zobacz też:

1. Wyrażenia regularne w Fish Shell:
    - https://fishshell.com/docs/current/index.html#regex
2. Dokumentacja Pup:
    - https://github.com/ericchiang/pup
3. Dokumentacja hxselect:
    - https://www.w3.org/Tools/HTML-XML-utils/README
4. Beautiful Soup dla Pythona:
   - https://www.crummy.com/software/BeautifulSoup/doc
5. Nokogiri dla Ruby:
   - https://nokogiri.org/tutorials/parsing_an_html_xml_document.html