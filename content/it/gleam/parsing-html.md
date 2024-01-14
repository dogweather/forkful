---
title:                "Gleam: Analizzazione di HTML"
simple_title:         "Analizzazione di HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Molti di noi sono incappati almeno una volta in un sito web con informazioni preziose che vogliamo estrarre, ma senza la possibilità di trovare un modo semplice per farlo. Grazie a Gleam, possiamo automatizzare il processo di parsing HTML, rendendo più facile e veloce l'estrazione dei dati che ci interessano.

## Come Fare

Il primo passo è includere il modulo `html` nel nostro file Gleam:

```Gleam
import html
```

Una volta incluso il modulo, possiamo utilizzare la funzione `parse` per analizzare il codice HTML di una pagina web. Ad esempio, vogliamo estrarre il titolo e la descrizione di un articolo di un sito di notizie:

```Gleam
html
|> parse("https://www.esempio.it/articolo")
|> html.element("h1")
|> html.child()
|> html.text() // Estrae il titolo del nostro articolo
```

L'output sarà il titolo dell'articolo come stringa. Per estrarre la descrizione, possiamo utilizzare ancora la funzione `parse` con una selezione più specifica:

```Gleam
html
|> parse("https://www.esempio.it/articolo")
|> html.element(".description") // Seleziona l'elemento con classe "description"
|> html.child()
|> html.text() // Estrae la descrizione del nostro articolo
```

È importante notare che alcuni siti web possono avere una struttura HTML più complessa, quindi potrebbe essere necessario utilizzare altri metodi di ricerca come `html.children()` o `html.descendants()` per ottenere i dati desiderati. È sempre una buona idea esaminare il codice HTML della pagina per avere un'idea della struttura e dei nomi degli elementi.

## Approfondimento

La funzione `parse` del modulo `html` utilizza il modulo `parse_html` che a sua volta si basa sulla libreria Erlang `html_parse`. Ciò significa che Gleam utilizza un parser HTML solido e altamente performante, rendendo il processo di analisi dei dati ancora più efficiente.

Inoltre, possiamo utilizzare l'algebra del tipo `Html.Node` per eseguire pattern matching più complessi e aggiungere una maggiore flessibilità nella nostra implementazione. Ad esempio, possiamo utilizzare `Html.element()` per selezionare un elemento specifico o `Html.attribute()` per estrarre un attributo di un elemento.

## Vedi Anche

- Documentazione ufficiale di Gleam sul modulo HTML: https://gleam.run/modules/html.html
- Esempi di utilizzo del parsing HTML con Gleam: https://github.com/gleam-lang/gleam_stdlib_examples/tree/master/html_parsing
- Articoli che approfondiscono il concetto di parsing HTML: https://www.html.it/guide/guida-al-parsing-html/