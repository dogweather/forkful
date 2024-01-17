---
title:                "Analisi di html"
html_title:           "Gleam: Analisi di html"
simple_title:         "Analisi di html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/parsing-html.md"
---

{{< edit_this_page >}}

Cosa è e perché: 

Il parsing HTML è il processo di analisi di un documento HTML per identificare estrarre i contenuti e la struttura del documento. I programmatori lo fanno per elaborare e manipolare i dati presenti in una pagina web.

Come fare:

```Gleam
import gleam/html/parser

html = """
<html>
  <head>
    <title>Titolo della pagina</title>
  </head>
  <body>
    <h1>Benvenuto nella pagina</h1>
    <p>Contenuto della pagina</p>
  </body>
</html>
"""

document = html
|> gleam/html/parse
|> match(_{
  Ok(p) -> 
    // Ottieni il titolo
    title =
      p.children
      |> List.head
      |> Parser.parent
      |> Parser.data
    // Ottieni il contenuto
    content =
      p.children
      |> List.last
      |> Parser.parent
      |> Parser.data
  Err(_) ->
    // Gestisci l'errore
})

```

Deep Dive:

Il parsing HTML è stato introdotto nel 1993 come parte della creazione del linguaggio HTML. Prima, i documenti HTML erano analizzati manualmente, ma ciò si è rivelato inefficace quando le pagine web sono diventate più complesse. Un'alternativa al parsing HTML è l'utilizzo di librerie di scraping, ma questa tecnica è più complessa e dipende dalla struttura del sito web.

See Also:

Per ulteriori informazioni sul parsing HTML in Gleam, puoi consultare la documentazione ufficiale della libreria [html_parser](https://gleam.run/lib/gleam/html_parser/). Inoltre, puoi approfondire l'argomento sui siti [W3Schools](https://www.w3schools.com/xml/dom_intro.asp) e [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction).