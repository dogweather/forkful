---
title:                "Analisi sintattica dell'html"
html_title:           "Fish Shell: Analisi sintattica dell'html"
simple_title:         "Analisi sintattica dell'html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Stai cercando un modo semplice e veloce per estrarre informazioni da una pagina web? Forse devi analizzare una pagina HTML per un progetto di data scraping o semplicemente vuoi automatizzare una qualche azione sul web. In entrambi i casi, il parsing HTML può essere un'ottima soluzione.

## Come Fare

Per iniziare, devi assicurarti di avere Fish Shell installato sul tuo sistema. Se non l'hai ancora fatto, puoi seguir il tutorial di installazione sul sito ufficiale [fishshell.com](https://fishshell.com) o utilizzare il tuo gestore di pacchetti preferito.

Una volta installato, puoi utilizzare Fish per eseguire il parsing di una pagina HTML usando il comando `curl` e la funzione `string split` integrata di Fish Shell.

```
Fish Shell
curl <url> | string split "<tag>"
```

Ad esempio, se volessi estrarre tutti i link presenti in una pagina web, potresti usare il seguente comando:

```
Fish Shell
curl https://www.example.com/ |string split "<a"
```

Questo dividerà il contenuto della pagina HTML in base al tag `<a`, creando una lista di link facilmente accessibile e utilizzabile.

## Approfondimento

Se vuoi ottenere risultati più precisi e dettagliati, puoi utilizzare una delle librerie di parsing HTML disponibili per Fish Shell. Ad esempio, puoi installare la libreria `crawler` usando il gestore di pacchetti `fisherman`, e poi utilizzarla per analizzare una pagina HTML, ad esempio per estrarre i titoli delle notizie da un sito di news.

```
Fish Shell
set -l crawler (fisherman lilyball/crawler)
curl https://www.newswebsite.com/ | $crawler 'div[class="news-title"]' | perl -pe's/<[^>]+>/ /g' | string trim
```

Questo esempio utilizza il comando `curl` per ottenere il contenuto della pagina, il comando `set` per definire una variabile contenente la libreria `crawler`, il filtro `perl` per rimuovere i tag HTML e infine la funzione `string trim` per pulire il risultato finale.

## Vedi Anche

- [Sito ufficiale di Fish Shell](https://fishshell.com)
- [Guida all'installazione di Fish Shell](https://fishshell.com/docs/current/index.html#install)
- [Libreria `crawler` per Fish Shell su GitHub](https://github.com/lilyball/crawler)