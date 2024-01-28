---
title:                "Gestione degli errori"
date:                  2024-01-26T00:50:08.644299-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"
programming_language: "Bash"
category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Gestire gli errori nella scrittura di script Bash significa anticipare dove le cose potrebbero andare storte e affrontare la situazione con grazia. Perché? Beh, mantiene il tuo script robusto e salva gli utenti dal grattarsi la testa quando le cose non funzionano come previsto.

## Come fare:

```Bash
#!/bin/bash

# Reindirizzamento di stderr in un file
grep "qualcosa" file.txt 2> errori.log

# Gestione degli errori con stati di uscita
if ! grep "qualcosa" file.txt; then
    echo "Ops, qualcosa è andato storto cercando 'qualcosa'."
    exit 1
fi

# Uso di un trap per pulire prima dell'uscita in caso di errore
pulizia() {
  echo "Pulizia dei file temporanei..."
  rm temp_*
}

trap pulizia ERR

# errore intenzionale: il file non esiste
cat temp_file.txt
```

Esempio di output quando si verifica un errore:

```
Pulizia dei file temporanei...
cat: temp_file.txt: Nessun file o directory
```

## Approfondimenti

La gestione degli errori nella scrittura di script Bash risale alle origini della shell Unix, dove script robusti e affidabili erano (e sono) vitali per l'amministrazione del sistema e l'automazione. Tradizionalmente, gli errori in Bash vengono gestiti verificando lo stato di uscita di un comando, che per convenzione restituisce 0 in caso di successo e un valore diverso da zero in caso di fallimento.

Bash ha introdotto il comando `trap` come incorporato, consentendo agli utenti di specificare comandi da eseguire su vari segnali o uscite degli script. Questo è utile per attività di pulizia o come meccanismo di gestione degli errori di ultima istanza.

C'è anche il comando `set`, che può cambiare il comportamento di Bash in caso di errori. Ad esempio, `set -e` farà terminare immediatamente uno script se un qualsiasi comando esce con uno stato diverso da zero, un modo per fallire rapidamente ed evitare errori a catena.

Alternative al gestore di errori incorporato di Bash includono l'ispezione esplicita dell'esistenza dei file, l'uso della sostituzione dei comandi o addirittura la scrittura delle proprie funzioni per gestire gli errori in modo più granulare.

Sebbene la gestione rigorosa degli errori possa a volte sembrare eccessiva per piccoli script, è una pratica che può risparmiare molto tempo nel debug e prevenire comportamenti inaspettati sia per te che per gli utenti.

## Vedi Anche

- Manuale di Bash sui Parametri della Shell: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- La sezione della Guida Avanzata alla Scrittura di Script Bash sulla Gestione degli Errori: https://www.tldp.org/LDP/abs/html/exit-status.html
- Una guida approfondita su `trap`: https://mywiki.wooledge.org/SignalTrap

Ricorda, la scrittura di script è una forma d'arte, e il modo in cui gestisci gli scivoloni e gli inciampi può rendere il tuo capolavoro più resiliente. Buona scrittura di script!
