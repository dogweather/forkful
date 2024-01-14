---
title:                "Elixir: Analisi di html"
simple_title:         "Analisi di html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Molti di noi si sono trovati nella situazione in cui si vuole accedere ai contenuti presenti su un sito web, ma questi non sono disponibili in un formato facile da leggere e utilizzare. Invece di copiare e incollare manualmente dati da una pagina web, i programmatori utilizzano spesso il parsing HTML per estrarre informazioni utili dai contenuti delle pagine web.

## Come fare

Per eseguire il parsing di una pagina HTML in Elixir, possiamo utilizzare la libreria Floki. Iniziamo importando la libreria nel nostro file Elixir:

```Elixir
import Floki
```

Possiamo quindi utilizzare la funzione `html_document/1` per analizzare una stringa HTML e ottenere un documento strutturato:

```Elixir
html = """
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>Elixir Blog</title>
</head>
<body>
  <h1>Benvenuti su questo blog!</h1>
  <div class="posts">
    <div class="post">
      <h2>Post 1</h2>
      <p>Benvenuti su questo blog, stiamo scrivendo di Elixir!</p>
    </div>
    <div class="post">
      <h2>Post 2</h2>
      <p>Oggi parleremo di parsing HTML con Elixir.</p>
    </div>
  </div>
</body>
</html>
"""

doc = html_document(html)
```

Una volta che abbiamo un documento, possiamo usarlo per ottenere informazioni specifiche dalla pagina HTML. Ad esempio, possiamo ottenere il titolo della pagina utilizzando la funzione `Floki.find/2`:

```Elixir
title = doc
  |> Floki.find("title")
  |> Floki.text_node()
  |> String.trim()

IO.puts(title) # Elixir Blog
```

Possiamo anche ottenere una lista di tutti i paragrafi presenti nella pagina utilizzando `Floki.find_all/2`:

```Elixir
paragraphs = doc
  |> Floki.find_all("p")
  |> Enum.map(fn(element) -> Floki.text_node(element) end)

IO.inspect(paragraphs) # ["Benvenuti su questo blog, stiamo scrivendo di Elixir!", "Oggi parleremo di parsing HTML con Elixir."]
```

## Approfondimento

Il parsing HTML è una tecnica molto utile in Elixir e ci permette di accedere facilmente ai dati presenti nelle pagine web. Tuttavia, è importante notare che questo approccio può essere influenzato da cambiamenti nella struttura del sito web, quindi è importante mantenere il codice aggiornato.

Inoltre, esistono altre librerie in Elixir che possono aiutarci a eseguire il parsing di HTML, come per esempio HTML-Parser o HXT. È importante fare una ricerca e vedere quale libreria si adatta meglio alle nostre esigenze.

## Vedi anche

- [Floki documentazione](https://hexdocs.pm/floki/readme.html)
- [Parsing HTML con Elixir](https://www.poeticoding.com/parsing-html-with-elixir/)
- [HTML-Parser](https://github.com/philss/elixir-html-parser)