---
date: 2024-01-20 17:55:24.572257-07:00
description: "Leggere gli argomenti della riga di comando consente agli script Bash\
  \ di ricevere parametri esterni, rendendoli pi\xF9 flessibili. I programmatori fanno\
  \ ci\xF2\u2026"
lastmod: '2024-03-13T22:44:43.614892-06:00'
model: gpt-4-1106-preview
summary: "Leggere gli argomenti della riga di comando consente agli script Bash di\
  \ ricevere parametri esterni, rendendoli pi\xF9 flessibili."
title: Lettura degli argomenti della riga di comando
weight: 23
---

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
