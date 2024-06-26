---
date: 2024-01-26 01:09:10.018671-07:00
description: 'Come fare: Creare una semplice funzione in Bash.'
lastmod: '2024-03-13T22:44:43.605034-06:00'
model: gpt-4-1106-preview
summary: Creare una semplice funzione in Bash.
title: Organizzazione del codice in funzioni
weight: 18
---

## Come fare:
Creare una semplice funzione in Bash:

```Bash
saluta() {
  echo "Ciao, $1!"
}
```

Usala chiamando la funzione con un parametro:

```Bash
saluta "Mondo"  # Output: Ciao, Mondo!
```

Le funzioni possono restituire valori utilizzando `return` per codici di stato numerici (non per restituire dati effettivi):

```Bash
somma() {
  return $(($1 + $2))
}

somma 3 4
echo $?  # Output: 7
```

Nota che `$?` cattura il valore di ritorno dell'ultimo comando, che è il risultato numerico di `somma`.

## Approfondimento
In Bash, le funzioni sono un modo per compartimentare il codice sin dalle prime versioni. Storicamente, l'uso delle funzioni è in linea con i principi di programmazione strutturata introdotti negli anni '60 per migliorare la qualità del codice.

Alternative alle funzioni includono lo sourcing dei file di script o l'uso di alias, ma questi non offrono lo stesso livello di modularità e riutilizzo.

Un dettaglio di implementazione degno di nota in Bash è che le funzioni sono cittadini di prima classe; non hanno un'apposita parola chiave di dichiarazione come `function` in altri linguaggi, anche se `function` è opzionale in Bash per leggibilità. Anche l'ambito delle funzioni è interessante: le variabili sono globali per impostazione predefinita a meno che non vengano dichiarate come locali, il che può portare a comportamenti imprevisti se non gestiti correttamente.

## Vedi Anche
- Manuale di Bash sulle Funzioni della Shell: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Guida avanzata agli script in Bash: https://tldp.org/LDP/abs/html/functions.html
- "Pro Bash Programming: Scripting the GNU/Linux Shell" per concetti e pratiche approfondite sugli script di funzione.
