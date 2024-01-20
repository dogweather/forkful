---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

Avviare un nuovo progetto di programmazione significa creare un nuove file, cartelle e documenti necessario per sviluppare un'applicazione software. Lo facciamo per organizzare il nostro lavoro, rendendo più facile la tracciabilità e la gestione.

## Come Fare:
Quando si lavora nell'ambiente di programmazione Bash, l'inizializzazione di un nuovo progetto può essere così semplice come creare una nuova directory e un file Bash all'interno di essa. Ecco un esempio pratico:

```Bash
mkdir NuovoProgetto
cd NuovoProgetto
touch mioScript.sh
```
Questo creerà la directory "NuovoProgetto" e il file "mioScript.sh". `touch` è un comando Unix utilizzato per creare un nuovo file vuoto.

## Approfondimento

Bash, nato nel 1989 da un progetto GNU, è un'alternativa allo storico shel Bourne. Bash si contraddistingue per la capacità di combinare funzioni comode per scripting come il completamento automatico dei comandi, l'editing del comando, la portabilità etc.

Ci sono molte altre shell di scripting disponibili, come Zsh, Fish, Ksh, ecc. Fanno più o meno le stesse cose, ma differiscono in termini di sintassi e funzionalità aggiuntive.

Avviare un nuovo progetto può includere molteplici compiti, non solo la creazione di nuova directory/files, come la scelta di un sistema di gestione versioni, la definizione di codici di stile, l'implementazione di test automatici e così via, a seconda dell'ampiezza e della complessità del progetto.

## Vedi Anche

- Bash Programming Guide: https://www.gnu.org/software/bash/manual/bash.html
- Che cos'è la shell di Unix?:https://it.wikipedia.org/wiki/Unix_shell
- Sistemi di controllo della versione: https://it.wikipedia.org/wiki/Version_control_system