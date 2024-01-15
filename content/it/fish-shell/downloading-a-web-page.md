---
title:                "Scaricare una pagina web"
html_title:           "Fish Shell: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore in erba o un esperto nella tecnologia, è possibile che tu abbia incontrato situazioni in cui hai bisogno di ottenere dati da una pagina web. Fortunatamente, il Fish Shell è uno strumento potente e versatile che può aiutarti in questo compito. Continua a leggere per scoprire come utilizzare il Fish Shell per scaricare una pagina web.

## Come fare

Per scaricare una pagina web utilizzando Fish Shell, segui questi semplici passaggi:

1. Apri il terminale e accedi alla directory in cui vuoi salvare la pagina web.
2. Digita il comando `fish`, seguito da uno spazio e l'URL della pagina web che desideri scaricare. Ad esempio: `fish https://www.miosito.com`.
3. Premi Invio e il Fish Shell inizierà a scaricare la pagina web nella directory corrente.

Ecco un esempio di codice e output:

```fish
fish https://www.miosito.com
```

```
Scaricamento: https://www.miosito.com [OK]
File salvato come index.html
```

Il Fish Shell utilizzerà il suo potentissimo strumento di download per scaricare la pagina web e salvare il file resultante con il nome "index.html". Tutto questo in una sola riga di codice!

## Approfondimento

Ora che sai come utilizzare il Fish Shell per scaricare una pagina web, ecco alcune informazioni aggiuntive che potrebbero esserti utili:

- Se vuoi specificare un nome diverso per il file, basta aggiungere il nome desiderato dopo l'URL. Ad esempio: `fish https://www.miosito.com pagina.html`.
- Puoi anche scaricare più pagine web contemporaneamente, basta separare gli URL con uno spazio.
- Se la pagina che stai cercando di scaricare richiede l'autenticazione, puoi usare il comando `fish -u username -p password URL` per specificare le credenziali necessarie.
- Il Fish Shell supporta anche il download di file tramite protocollo FTP utilizzando il comando `fish -P ftp://url`. 

## Vedi anche

- [Fish Shell homepage](https://fishshell.com/): visita il sito ufficiale del Fish Shell per ulteriori informazioni e documentazione.
- [10 comandi essenziali per iniziare con Fish Shell](https://ostechnix.com/10-essential-commands-get-started-fish-shell/): un articolo che spiega 10 comandi fondamentali per utilizzare il Fish Shell.