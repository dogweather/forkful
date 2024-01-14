---
title:    "Bash: Ottenere la data attuale"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data attuale è un dato importante in molte applicazioni di programmazione, dal debug al logging. Conoscere come ottenere la data corrente è fondamentale per la gestione del tempo all'interno del tuo script Bash.

## Come Fare

Per ottenere la data attuale in Bash, è possibile utilizzare il comando `date`. Un semplice esempio di codice potrebbe essere il seguente:

```Bash
now=$(date +%c)
echo "La data attuale è: $now"
```

In questo esempio, stiamo utilizzando il simbolo `%c` per ottenere la data e l'ora completa in un formato leggibile. È possibile modificare il formato secondo le proprie esigenze utilizzando altri simboli, come `%Y` per l'anno o `%b` per il mese abbreviato.

L'output di questo script sarebbe qualcosa del genere:

```
La data attuale è: Tue Sep 28 11:20:42 CEST 2021
```

## Approfondimento

Oltre al comando `date`, Bash dispone di altre opzioni per ottenere la data attuale. Ad esempio, si può utilizzare il comando `printf` insieme a una stringa di formato per ottenere la data formattata secondo le proprie esigenze.

Un'altra opzione è utilizzare le variabili di sistema `TODAY` o `NOW`, che contengono automaticamente la data attuale in un formato predefinito.

Inoltre, è possibile impostare delle variabili personalizzate con la data attuale utilizzando il comando `declare` e l'opzione `-p`, come in questo esempio:

```Bash
declare today="$(date)"
declare now="$(date +%H:%M:%S)"
```

## Vedi Anche

- Comando `date` su Linux command library: https://linuxcommandlibrary.com/man/date
- Tutorial su come utilizzare il comando `date` in Bash: https://linuxize.com/post/bash-date-command/
- Approfondimento sulle variabili di sistema `NOW` e `TODAY`: https://www.howtogeek.com/442136/how-to-use-date-time-variables-in-bash/
- Guida al comando `printf` in Bash: https://www.computerhope.com/unix/bash/printf.htm