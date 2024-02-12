---
title:                "Analysering av HTML"
date:                  2024-02-03T17:59:57.885317-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av HTML i Go innebærer å analysere innholdet i HTML-filer for å ekstrahere data, manipulere strukturen, eller for å konvertere HTML til andre formater. Programmerere gjør dette for web scraping, templating, og datautvinning, ved å utnytte de sterke samtidighetsfunksjonene til Go for effektiv behandling av store mengder nettsider.

## Hvordan:

For å parse HTML i Go, bruker du vanligvis `goquery`-pakken eller standard bibliotekets `net/html`-pakke. Her er et grunnleggende eksempel som bruker `net/html` for å trekke ut alle lenker fra en nettside:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Hent HTML-dokument
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Parse HTML-dokumentet
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Funksjon for å rekursivt traversere DOM
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // Traverser DOM
    f(doc)
}
```

Eksempel på utdata (med antakelsen om at `http://example.com` inneholder to lenker):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Denne koden forespør en HTML-side, parser den, og traverserer rekursivt DOM for å finne og skrive ut `href`-attributter av alle `<a>`-tagger.

## Dypdykk

`net/html`-pakken gir grunnleggende verktøy for parsing av HTML i Go, ved direkte implementering av tokeniserings- og trebygningsalgoritmer spesifisert av HTML5-standarden. Denne lavnivåtilnærmingen er kraftig, men kan være ordrik for komplekse oppgaver.

I kontrast tilbyr den eksterne `goquery`-pakken, inspirert av jQuery, et høyere nivå grensesnitt som forenkler DOM-manipulasjon og -traversering. Den lar utviklere skrive kortfattet og uttrykksfull kode for oppgaver som elementvalg, attributtekstraksjon og innholdsmanipulasjon.

Men, `goquery` sin bekvemmelighet kommer på bekostning av en ekstra avhengighet og potensielt tregere ytelse på grunn av sitt abstraksjonslag. Valget mellom `net/html` og `goquery` (eller andre parsing biblioteker) avhenger av de spesifikke kravene til prosjektet, som behovet for ytelsesoptimalisering eller brukervennlighet.

Historisk har HTML-parsing i Go utviklet seg fra grunnleggende strengoperasjoner til sofistikert DOM-tremanipulasjon, noe som reflekterer språkets voksende økosystem og samfunnets etterspørsel etter robuste verktøy for web scraping og dataekstraksjon. Til tross for de innebygde mulighetene, fremhever prevalensen av tredjepartbiblioteker som `goquery` Go-fellesskapets preferanse for modulær, gjenbrukbar kode. Imidlertid, for ytelseskritiske applikasjoner, kan programmerere fortsatt foretrekke `net/html`-pakken eller til og med ty til regex for enkle parsingoppgaver, med tanke på de iboende risikoene og begrensningene ved regex-basert HTML-parsing.
