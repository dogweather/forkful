---
title:                "Ottenere la data attuale"
html_title:           "Bash: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Perché

Ci sono molte ragioni per cui uno potrebbe voler ottenere la data corrente su Bash. Forse stai creando uno script automatizzato che deve eseguire determinate azioni in base alla data, o forse vuoi solo sapere in che giorno ci siamo.

##Come Fare

È facile ottenere la data corrente su Bash utilizzando il comando "date". Basta digitare il seguente comando nel terminale e premere Invio:

```Bash
date
```

Se vuoi visualizzare solo la data senza l'orario, puoi utilizzare l'opzione "-d" seguita dal formato desiderato. Ad esempio:

```Bash
date -d "%m/%d/%Y"
```
Questo visualizzerà la data nel formato mese/giorno/anno.

##Approfondimento

Il comando "date" è molto versatile e permette di ottenere informazioni dettagliate sulla data, come il numero di settimane trascorse dall'inizio dell'anno o il numero di giorni rimasti fino alla fine del mese.

Per ottenere la settimana corrente, puoi utilizzare l'opzione "-W":

```Bash
date -d "%YW%W"
```

Questo ti darà un output del tipo "2020W46", il che significa che siamo nella 46° settimana dell'anno 2020.

Per ulteriori informazioni sulle opzioni disponibili e su come personalizzare il formato della data, puoi consultare la documentazione ufficiale del comando "date".

##Vedi Anche

- Documentazione ufficiale del comando "date": https://man7.org/linux/man-pages/man1/date.1.html
- Ulteriori esempi di utilizzo del comando "date": https://www.tecmint.com/date-command-examples/
- Tutorial su come utilizzare il comando "date" su Bash: https://www.howtogeek.com/442093/how-to-use-the-date-command-on-linux/