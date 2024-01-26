---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:31:48.404884-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Il parsing HTML implica leggere e interpretare il codice HTML per estrarne dati o struttura. I programmatori lo fanno per automatizzare l'interazione con il web, estrarre informazioni o testare l'integrità dei siti.

## How to:
Gleam non ha una libreria standard per il parsing HTML al momento, quindi dobbiamo usare un'estensione. Supponiamo che `gleam_html_parser` sia la nostra scelta ipotetica. Mettiamo mani al codice:

```gleam
import gleam_html_parser

pub fn main() {
  let html = "<p>Hello, Gleam!</p>"
  let parsed_data = gleam_html_parser.parse(html)

  case parsed_data {
    Ok(nodes) -> 
      nodes
        |> List.map(fn(node) { 
          node.text() 
        })
        |> io.println
    Error(_) -> 
      io.println("Failed to parse HTML")
  }
}
```

Output:
```
["Hello, Gleam!"]
```

## Deep Dive:
Parsing HTML risale al tempo quando il web era ancora giovane. Moduli come Python's `BeautifulSoup` hanno reso più semplice scavare nei segreti del HTML. In alternativa, ci sono parser basati su espressioni regolari, ma attenzione: queste sono fragili e possono fallire con HTML non ben formato.

Gleam è un linguaggio giovane con un ecosistema in crescita, quindi opzioni come `gleam_html_parser` (ipotetica) potrebbero essere limitate o in via di sviluppo. Il parsing HTML in Gleam avrebbe una forte enfasi sulla sicurezza del tipo, sfruttando le sue funzionalità di pattern matching per un parsing robusto e affidabile.

In altre lingue si usano spesso libreria come `lxml` o `Jsoup`, ma in Gleam fare parsing significa anche lavorare con la concorrenza e pattern matching che il linguaggio offre, per un approccio funzionale e più espressivo.

## See Also:
- [Gleam's official website](https://gleam.run)
- [Gleam Standard Library Documentation](https://hexdocs.pm/gleam_stdlib/)
- [HTML5 Parsing Algorithm Spec](https://html.spec.whatwg.org/multipage/parsing.html)
