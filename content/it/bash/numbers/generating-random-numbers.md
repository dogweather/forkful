---
date: 2024-01-27 20:32:47.358841-07:00
description: "Come fare: In Bash, la variabile `$RANDOM` \xE8 la soluzione prediletta\
  \ per generare numeri casuali. Ogni volta che la si richiama, Bash fornisce un intero\u2026"
lastmod: '2024-03-13T22:44:43.593432-06:00'
model: gpt-4-0125-preview
summary: "In Bash, la variabile `$RANDOM` \xE8 la soluzione prediletta per generare\
  \ numeri casuali."
title: Generazione di numeri casuali
weight: 12
---

## Come fare:
In Bash, la variabile `$RANDOM` è la soluzione prediletta per generare numeri casuali. Ogni volta che la si richiama, Bash fornisce un intero pseudocasuale compreso tra 0 e 32767. Esploriamo alcuni esempi pratici:

```Bash
# Uso base di $RANDOM
echo $RANDOM

# Generazione di un numero casuale in un intervallo specificato (qui 0-99)
echo $(( RANDOM % 100 ))

# Generazione di un numero casuale più "sicuro", adatto per password o chiavi
# Utilizzo di /dev/urandom con il comando od
head -c 8 /dev/urandom | od -An -tu4

# Inizializzazione di RANDOM per la riproducibilità
RANDOM=42; echo $RANDOM
```

Output di esempio (nota: l'output effettivo varierà poiché i numeri sono casuali):
```Bash
16253
83
3581760565
17220
```

## Approfondimento
Il meccanismo dietro `$RANDOM` di Bash genera numeri pseudocasuali, il che significa che seguono un algoritmo e possono, in teoria, essere prevedibili - una potenziale falla di sicurezza per le applicazioni che richiedono una vera imprevedibilità. Le applicazioni crittografiche moderne di solito richiedono casualità derivata da fenomeni fisici o da hardware progettato appositamente per generare dati casuali, come `/dev/urandom` o `/dev/random` in Linux, che raccolgono rumore ambientale.

Per compiti casuali o non critici per la sicurezza, `$RANDOM` è sufficiente e offre il vantaggio della semplicità. Tuttavia, per scopi crittografici o dove la qualità della casualità è critica, gli sviluppatori dovrebbero guardare verso altri strumenti e linguaggi progettati con la crittografia in mente, come OpenSSL o i linguaggi di programmazione con librerie robuste per la generazione di numeri casuali.

Mentre `$RANDOM` di Bash svolge la sua funzione negli script che richiedono numeri casuali di base, le sue limitazioni dovrebbero indirizzare gli sviluppatori verso soluzioni più robuste per applicazioni dove la qualità o la sicurezza della casualità sono importanti.
