---
title:                "Haskell: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché Scaricare una Pagina Web in Haskell?

Scaricare pagine web è un'operazione comune in molti progetti di programmazione. In Haskell, è possibile farlo in modo semplice e flessibile grazie ad alcune librerie apposite. In questo articolo, mostrerò come scaricare una pagina web utilizzando il linguaggio funzionale Haskell e come approfondire questo concetto.

## Come Fare

Per prima cosa, dovrai installare la libreria `http-conduit` utilizzando `cabal`:

```Haskell
cabal update
cabal install http-conduit
```

Una volta installata, è possibile importare la libreria nel tuo file Haskell utilizzando il comando `import Network.HTTP.Conduit`.

Ora, per scaricare una pagina web, possiamo utilizzare la funzione `simpleHttp` che accetta come argomento un URL e ci restituisce il contenuto della pagina come una stringa:

```Haskell
paginaWeb <- simpleHttp "https://www.example.com"
```

Possiamo anche aggiungere degli header personalizzati alla nostra richiesta utilizzando la funzione `requestHeaders`:

```Haskell
let header = ("User-Agent", "Haskell/1.0")
paginaWeb <- simpleHttp $ setRequestHeader header "https://www.example.com"
```

Una volta scaricata la pagina web, possiamo utilizzare la funzione `unpack` per convertire la stringa in un formato utilizzabile:

```Haskell
let pagina = unpack paginaWeb
```

Ora possiamo stampare il contenuto della pagina utilizzando la funzione `putStrLn`:

```Haskell
putStrLn pagina
```

## Approfondimenti

Oltre a `http-conduit`, esistono altre librerie che consentono di scaricare pagine web in Haskell, come ad esempio `curl`, `wget`, `HsWebKit`, solo per citarne alcune. Ognuna di queste librerie ha i propri vantaggi e svantaggi, quindi è importante fare ricerche per trovare quella che meglio si adatta alle tue esigenze.

Per ulteriori informazioni sul download di pagine web in Haskell, ti consiglio di leggere il seguente articolo su [HaskellWiki](https://wiki.haskell.org/Downloading_web_pages).

## Vedi Anche

* [HaskellWiki - Downloading web pages](https://wiki.haskell.org/Downloading_web_pages)
* [Hackage - http-conduit](https://hackage.haskell.org/package/http-conduit)
* [GitHub - curl](https://github.com/bumptech/curl-hs)
* [GitHub - wget](https://github.com/jmcarthur/haskell-wget)
* [Hackage - HsWebKit](https://hackage.haskell.org/package/HsWebKit)