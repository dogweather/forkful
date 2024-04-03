---
date: 2024-01-19
description: 'How to: (Come fare:) Ecco alcuni esempi di base.'
lastmod: '2024-03-13T22:44:43.588135-06:00'
model: unknown
summary: Ecco alcuni esempi di base.
title: Utilizzo delle espressioni regolari
weight: 11
---

## How to: (Come fare:)
Ecco alcuni esempi di base:

```Bash
# Trova tutte le occorrenze di 'casa' in un file
grep 'casa' nomi_case.txt

# Sostituisce 'ciao' con 'salve' in tutte le righe di un file
sed -i 's/ciao/salve/g' saluti.txt

# Estrae tutti gli indirizzi email da un file, usando un'expressione regolare
grep -oP '([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})' email.txt
```

Output per il terzo comando potrebbe essere:
```
mario.rossi@email.it
luigi.bianchi@email.com
```

## Deep Dive (Approfondimento)
Le regex esistono dai primi anni '50, sviluppate da matematici come Stephen Kleene. Alternative moderne incluiscono parser e linguaggi specifici di dominio (DSL), anche se le regex restano un'utilità per la loro velocità e espressività. Vari linguaggi implementano regex in modi leggermente diversi, perciò è importante verificare la compatibilità con Bash.

## See Also (Vedi Anche)
- [GNU Grep Documentation](https://www.gnu.org/software/grep/manual/grep.html)
- [Sed - An Introduction and Tutorial by Bruce Barnett](https://www.grymoire.com/Unix/Sed.html)
- [Regex101: Online regex tester and debugger](https://regex101.com/) per testare le tue espressioni regolari online.
