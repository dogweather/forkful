---
title:    "Fish Shell: Ottenere la data corrente"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché
Se stai scrivendo un programma che ha bisogno di conoscere la data attuale, come ad esempio un bot che pubblica messaggi giornalieri o una funzione di salvataggio automatico, avrai bisogno di un modo per ottenere la data corrente all'interno del tuo codice. In questo post, ti mostrerò come farlo utilizzando il Fish Shell!

## Come Fare
Per ottenere la data corrente utilizzando il Fish Shell, possiamo utilizzare il comando "date". Questo comando ci permette di visualizzare la data e l'orario attuale in diversi formati. Ecco un esempio:

```Fish Shell
date
```

Questo ci darà un output simile a questo:

```
mer giu 16 14:30:00 CEST 2021
```

Possiamo anche specificare il formato desiderato utilizzando le opzioni del comando "date". Ad esempio, per ottenere la data nel formato "gg/mm/aaaa", possiamo utilizzare il seguente comando:

```Fish Shell
date +"%d/%m/%Y"
```

Questo ci darà un output come questo:

```
16/06/2021
```

## Approfondimento
Il comando "date" utilizza il clock time del sistema per ottenere la data corrente. Ci sono anche altre opzioni disponibili per il formato della data, che puoi scoprire esplorando il manuale di Fish Shell. Inoltre, puoi anche utilizzare il comando "cal", che visualizza un calendario del mese corrente, per ottenere la data corrente in base al calendario.

## Vedi anche
- [Fish Shell - Documentazione ufficiale](https://fishshell.com/docs/current/index.html)
- [Guida rapida alla riga di comando di Fish Shell](https://dev.to/lucpattyn/guida-rapida-alla-riga-di-comando-di-fish-shell-3ack)
- [Come utilizzare Fish Shell come shell predefinita su macOS](https://www.micheledallatorre.com/fish-shell-per-macos/)

Grazie per aver letto questo post e spero che ora tu sappia come ottenere la data corrente utilizzando il Fish Shell!