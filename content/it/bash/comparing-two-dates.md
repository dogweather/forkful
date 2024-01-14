---
title:                "Bash: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Perché

Comparare due date può essere utile per controllare la cronologia di eventi, pianificare attività future o semplicemente tenere traccia del tempo. In Bash, esistono vari metodi per confrontare due date e trovare la differenza tra di esse.

##Come Fare

Per iniziare, è necessario conoscere il formato delle date utilizzato nel vostro sistema. In genere, nel sistema operativo Linux, il seguente formato viene utilizzato:

```Bash
date +%Y-%m-%d
```

Questo restituisce l'anno, il mese e il giorno corrente nel formato AAAA-MM-GG (ad esempio: 2021-10-12). Una volta che avete a disposizione questa informazione, è possibile utilizzare comandi come `expr` o `bc` per confrontare le date. Ad esempio, per confrontare se una data è successiva o precedente a un'altra data, è possibile utilizzare il seguente codice:

```Bash
if [ $(date --date "2021-10-12" +%s) -gt $(date --date "2021-10-11" +%s) ]; then
	echo "La prima data è successiva alla seconda."
fi
```

In questo esempio, utilizziamo il comando `date` per convertire le date in secondi e quindi confrontarle con l'aiuto del comando `if` in una semplice istruzione Bash. È importante notare che il formato della data deve essere lo stesso per entrambe le date per poter essere confrontate correttamente.

##Deep Dive

Ci sono anche altre opzioni e funzioni che si possono utilizzare per confrontare due date in Bash, come ad esempio `diff` per trovare la differenza nei giorni o nelle ore, o `dateutils` per aiutare a gestire date in diversi formati. È anche possibile convertire le date nel comune formato Unix timestamp utilizzato per confrontare facilmente le date.

Inoltre, come menzionato in precedenza, è fondamentale conoscere il formato delle date del vostro sistema e sapere che alcuni comandi e funzioni possono variare a seconda della distribuzione di Linux utilizzata. È sempre consigliabile consultare la documentazione ufficiale per avere informazioni precise e aggiornate.

##Vedi Anche

- [Comandi Unix per la gestione delle date](https://www.computerhope.com/unix/udate.htm)
- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)
- [Guida rapida allo scripting Bash](https://www.tutorialspoint.com/unix/bash_tutorial.pdf)