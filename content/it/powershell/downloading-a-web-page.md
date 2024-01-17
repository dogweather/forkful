---
title:                "Scaricare una pagina web"
html_title:           "PowerShell: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scaricare una pagina web significa ottenere il contenuto di una pagina internet sul tuo computer. I programmatori lo fanno per poter manipolare e analizzare i dati contenuti nella pagina, o per automatizzare processi come il crawling di siti web.

## Come:
Il modo più semplice per scaricare una pagina web in PowerShell è utilizzare il cmdlet `Invoke-WebRequest`. Basta specificare l'URL della pagina come parametro e salvare il risultato in una variabile.
```
$paginaWeb = Invoke-WebRequest -Uri 'https://www.miapagina.com'
```
Per accedere al contenuto della pagina, è possibile utilizzare la proprietà `Content` della variabile:
```
$paginaWeb.Content
```
In questo modo otterrai il codice HTML della pagina, che puoi poi manipolare e analizzare secondo le tue necessità.

## Approfondimenti:
La possibilità di scaricare una pagina web attraverso il codice è stata resa possibile grazie al protocollo HTTP, sviluppato negli anni '90. Se invece vuoi utilizzare questa funzionalità in un linguaggio diverso da PowerShell, puoi farlo utilizzando librerie come `curl` o `wget`.
Nel codice sopra mostrato, la pagina viene scaricata utilizzando il metodo `GET` di HTTP. Se invece hai bisogno di passare dei parametri nella richiesta, puoi utilizzare il metodo `POST` e specificare i parametri nella proprietà `Body`.

## Vedi anche:
- [Documentazione ufficiale di Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [Cosa è il protocollo HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Overview)