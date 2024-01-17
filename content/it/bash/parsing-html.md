---
title:                "Parsing html"
html_title:           "Bash: Parsing html"
simple_title:         "Parsing html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Il parsing HTML si riferisce al processo di analisi e interpretazione di codice HTML. I programmatori lo fanno per estrarre informazioni specifiche da una pagina web o per manipolare il suo contenuto. Questo può essere fatto utilizzando strumenti o script appositamente progettati in Bash per automatizzare il processo di parsing.

## Come fare:
Il seguente esempio mostra come utilizzare lo strumento `lynx` per eseguire il parsing di un sito web e prendere il suo titolo:

```Bash
lynx -dump -head "https://www.esempio.com" | grep "Title:" | awk '{print $2}'
```

Output: "Titolo del sito web"

## Approfondimento:
Il parsing HTML è stato inventato nel 1993 da Tim Berners-Lee, l'inventore del World Wide Web. Mentre Bash non è il linguaggio più comune per il parsing HTML, può essere una buona scelta per i programmatori con familiarità con esso che vogliono automatizzare alcune operazioni di parsing. Ci sono anche alternative come cURL e wget che possono essere utilizzati per eseguire il parsing in Bash. Per implementare il parsing HTML in Bash, è necessario utilizzare strumenti o script appositi, in quanto Bash non è nativamente progettato per questa funzione.

## Vedi anche:
- [Documentazione di lynx](http://lynx.browser.org/)
- [Introduzione al parsing HTML in Bash](https://www.tecmint.com/parsing-html-file-in-linux/)
- [Altre alternative per il parsing HTML in Bash](https://stackoverflow.com/questions/18227949/parsing-html-in-bash-shell)