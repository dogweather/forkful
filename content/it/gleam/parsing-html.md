---
title:                "Analisi di html."
html_title:           "Gleam: Analisi di html."
simple_title:         "Analisi di html."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Se hai mai lavorato con pagine web, probabilmente hai incontrato il formato HTML. Potresti voler analizzare un sito web per raccogliere dati o forse vuoi analizzare il contenuto di una pagina web per eseguire azioni specifiche. L'analisi di HTML è un'abilità preziosa per creare script automatizzati e prendere decisioni informate.

## Come Fare

Usando la libreria Gleam, analizzare HTML è facile e divertente. Segui questi semplici passaggi per iniziare:

1. Installa la libreria Gleam nel tuo progetto usando `gleam install gleam/html`
2. Importa la libreria nel tuo file `main.gleam` usando `import gleam/html`
3. Usa la funzione `Html.parse` per analizzare una stringa di HTML in una struttura dati di tipo `Html.document`
4. Puoi usare i metodi della struttura dati `Html.document` per estrarre informazioni e navigare attraverso la struttura HTML in modo programmatico
5. Ecco un esempio di come analizzare il contenuto di una pagina web estrarre tutti i link:

```Gleam
import gleam/io
import gleam/html

let html = "<html><body><a href='https://gleam.run'>Gleam</a><a href='https://www.google.com'>Google</a></body></html>"
let document = Html.parse(html)

let links = document
  |> Html.Node.find_all("a")
  |> List.map(Html.Node.get_attribute("href"))

gleam/io.println(links)
```

Questo dovrebbe stampare `[Ok("https://gleam.run"), Ok("https://www.google.com")]`.

## Approfondimento

La libreria Gleam offre molte altre funzioni utili per analizzare HTML, come `Html.Node.get_text` per ottenere il testo di un elemento HTML e `Html.Node.find` per trovare un elemento specifico in base al suo id o classe. Inoltre, puoi utilizzare il modulo `gleam/html/parser` per analizzare HTML dalle pagine web direttamente. La libreria è ben documentata quindi assicurati di consultare la documentazione ufficiale per ulteriori informazioni.

## Vedi Anche

- Documentazione ufficiale di Gleam su come analizzare HTML: https://gleam.run/libraries/html
- Esempi di codice di analisi HTML usando Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/html_parsing.gleam
- Tutorial su come analizzare HTML con Gleam: https://gleam.run/tutorials/html_parsing