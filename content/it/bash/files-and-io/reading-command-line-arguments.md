---
title:                "Lettura degli argomenti della riga di comando"
aliases:
- /it/bash/reading-command-line-arguments.md
date:                  2024-01-20T17:55:24.572257-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere gli argomenti della riga di comando consente agli script Bash di ricevere parametri esterni, rendendoli più flessibili. I programmatori fanno ciò per personalizzare l'esecuzione degli script in base agli input forniti dall'utente.

## Come fare:
```Bash
#!/bin/bash
# Stampa il primo argomento
echo "Il primo argomento è: $1"

# Stampa tutti gli argomenti
echo "Tutti gli argomenti: $@"

# Conta gli argomenti passati
echo "Numero di argomenti: $#"
```
Output esemplificativo quando si lancia `./script.sh uno due tre`:
```Bash
Il primo argomento è: uno
Tutti gli argomenti: uno due tre
Numero di argomenti: 3
```

## Approfondimenti
In Bash, gli argomenti della riga di comando sono stati usati fin dalla sua creazione negli anni '80, come parte del progetto GNU. Si trattava di una necessità per automatizzare task e gestire script dinamici. Alternative per la lettura di argomenti includono l'uso di `getopts` o `optarg` per opzioni più complesse. Implementazione dettagliata: `$0` rappresenta il nome dello script, `$1` al `$9` rappresentano gli argomenti dalla prima alla nona posizione, `${10}` e oltre per argomenti successivi al nono, `$@` per tutti gli argomenti come singole parole, e `$*` tutti gli argomenti come una singola stringa.

## Vedi Anche
- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash Guide for Beginners: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/
