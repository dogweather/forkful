---
title:                "Scaricare una pagina web"
html_title:           "Bash: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Il download di una pagina web consiste nel salvare una copia di una pagina web sul proprio computer. I programmatori lo fanno per vari motivi, come ad esempio per analizzare il codice di una pagina, per utilizzarla come riferimento o per ottenere dati da essa.

## Come fare:
Un esempio semplice per scaricare una pagina web utilizzando Bash è utilizzare il comando `curl` seguito dall'URL della pagina. Ad esempio: 
```Bash
curl https://www.example.com
```
L'output di questo comando mostrerà il codice HTML della pagina web richiesta.

## Approfondimento:
Scaricare una pagina web utilizzando Bash è possibile grazie al protocollo HTTP (Hypertext Transfer Protocol). Questo protocollo è stato sviluppato negli anni '90 ed è stato uno dei primi metodi utilizzati per accedere alle pagine web. Esistono anche altri strumenti per il download di pagine web, come ad esempio il comando `wget`, ma `curl` è quello più utilizzato dai programmatori.

## Risorse utili:
- Documentazione ufficiale di Bash: [https://www.gnu.org/software/bash/](https://www.gnu.org/software/bash/)
- Documentazione di `curl`: [https://curl.haxx.se/docs/manual.html](https://curl.haxx.se/docs/manual.html)
- Documentazione di `wget`: [https://www.gnu.org/software/wget/](https://www.gnu.org/software/wget/)