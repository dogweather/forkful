---
title:                "Analisi di HTML"
html_title:           "Elixir: Analisi di HTML"
simple_title:         "Analisi di HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con dati da fonti web, probabilmente hai incontrato il problema di dover estrarre informazioni da pagine HTML. Ecco perché imparare a parsare l'HTML può aumentare la tua efficienza e facilitare la gestione dei dati estratti dai siti web.

## Come Fare

In Elixir, esistono diversi modi per parsare l'HTML, ma il più comune è utilizzare la libreria Floki. Per iniziare, installa la libreria con il comando `mix`:

```elixir
mix deps.get floki
```

Una volta installata, puoi utilizzare la funzione `Floki.parse` per caricare il codice HTML dal sito web che desideri parsare:

```elixir
html = Floki.parse("""
<!DOCTYPE html>
<html>
  <head>
    <title>Titolo Pagina</title>
  </head>
  <body>
    <h1>Benvenuto!</h1>
    <ul>
      <li>Elemento 1</li>
      <li>Elemento 2</li>
      <li>Elemento 3</li>
    <ul>
  </body>
</html>
""")
```

Una volta caricato, puoi utilizzare le funzioni di Floki per navigare all'interno dell'HTML e estrarre i dati di tuo interesse. Ad esempio, per estrarre tutti i titoli `h1` nella pagina, puoi utilizzare la funzione `Floki.find` e l'operatore `~>`:

```elixir
Floki.find(html, "h1")
# output: ["Benvenuto!"]
```

È anche possibile utilizzare selettori CSS per specificare esattamente quale elemento si desidera estrarre. Ad esempio, per ottenere tutti gli elementi `li` dalla lista, puoi utilizzare il selettore CSS `ul li`:

```elixir
Floki.find(html, "ul li")
# output: ["Elemento 1", "Elemento 2", "Elemento 3"]
```

Inoltre, Floki offre funzioni per ottenere attributi specifici degli elementi, come l'attributo `href` di un link:

```elixir
Floki.attribute(html, "a", "href")
# output: ["https://www.miopropriositoweb.com"]
```

## Deep Dive

Una delle caratteristiche più potenti di Floki è la possibilità di utilizzare espressioni XPath per navigare all'interno dell'HTML. Invece di utilizzare selettori CSS, puoi utilizzare XPath per selezionare elementi in base al loro percorso nella struttura del documento HTML. Ad esempio, per ottenere la lista degli elementi `li` dalla lista, puoi utilizzare l'espressione XPath `//ul/li`:

```elixir
Floki.find(html, "//ul/li")
# output: ["Elemento 1", "Elemento 2", "Elemento 3"]
```

In più, Floki offre anche funzioni per manipolare l'HTML, come ad esempio `Floki.parent` per ottenere il padre di un determinato elemento, o `Floki.html_to_text` per convertire il codice HTML in testo semplice.

## Vedi Anche

- [Documentazione ufficiale di Floki](https://hexdocs.pm/floki/Floki.html)
- [XPath Tutorial su w3schools](https://www.w3schools.com/xml/xpath_intro.asp)