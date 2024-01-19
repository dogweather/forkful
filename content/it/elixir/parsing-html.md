---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché? 

L'analisi del HTML (parsing HTML) consiste nel decomporre e comprendere un documento HTML. I programmatori lo fanno per estrarre informazioni utili da pagine web o manipolare la struttura di un documento HTML.

## Come si fa:
Elixir offre molte soluzioni per l'analisi del HTML. Un esempio è usando la libreria Floki. Qui c'è un esempio:

```Elixir
def deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end
```
Per estrarre un elemento dalla pagina:

```Elixir
def parse_html_page() do
  {:ok, body} = HTTPoison.get("https://example.com")
  Floki.find(body.body, "div.article")
end
```
Questo codice restituirà tutti gli elementi `div` con classe `article` dalla pagina web.

## Approfondimenti
Sure, l'analisi del HTML sembra semplice il questi giorni grazie a Elixir e Floki, ma non è sempre stato così. In passato, l'analisi del HTML era un'operazione dolorosa, spesso dipendeva da espressioni regolari che non erano affidabili.

Un alternativa a Floki potrebbe essere Mochiweb, un'altra libreria Elixir. Offre un set di funzionalità simile ma la scelta tra i due dipende spesso dal caso d'uso e dalle preferenze personali.

Per quanto riguarda i dettagli implementativi, Floki sfrutta una combinazione di trasformazioni a livello di stringa e alberi di sintassi astratta (AST) per il parsing e la manipolazione di documenti HTML.

## Vedi Anche
Per ulteriori informazioni su questo argomento, ecco alcuni link utili:

1. [Guida ufficiale Floki](https://hexdocs.pm/floki/readme.html)
2. [La documentazione di HTTPoison](https://hexdocs.pm/httpoison/readme.html)
3. [Progetto GitHub di Mochiweb](https://github.com/mochi/mochiweb)

Ricorda, il mestiere di un programmatore è fatto di apprendimento continuo. Buona programmazione!