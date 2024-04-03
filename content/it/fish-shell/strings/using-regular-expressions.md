---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:47.108031-07:00
description: "Le espressioni regolari (regex) in Fish Shell ti permettono di cercare,\
  \ correlare e manipolare stringhe basate su schemi specifici. I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.846005-06:00'
model: gpt-4-0125-preview
summary: Le espressioni regolari (regex) in Fish Shell ti permettono di cercare, correlare
  e manipolare stringhe basate su schemi specifici.
title: Utilizzo delle espressioni regolari
weight: 11
---

## Cos'è e perché?

Le espressioni regolari (regex) in Fish Shell ti permettono di cercare, correlare e manipolare stringhe basate su schemi specifici. I programmatori utilizzano le regex per compiti come la validazione di input, l'analisi e l'elaborazione di testi perché offrono un modo compatto e potente per specificare schemi di testo complessi.

## Come fare:

Sebbene Fish Shell stesso non abbia un comando integrato per le regex, utilizza efficacemente comandi esterni come `grep`, `sed` e `awk` che supportano le regex, permettendoti di incorporare operazioni regex nei tuoi script.

### Ricerca di Modelli di Base con `grep`
Cerca righe in un file che corrispondono a un modello:

```fish
grep '^[0-9]+' myfile.txt
```

Questo comando trova le righe che iniziano con uno o più cifre in `myfile.txt`.

### Estrazione e Sostituzione con `sed`
Estrai numeri di telefono da un file:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Sostituisci tutte le occorrenze di "foo" con "bar" in `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### Utilizzo di `string` per le Regex di Base
Il comando `string` di Fish Shell supporta semplici operazioni regex come corrispondenza e sostituzione:

Corrispondenza di un modello in una stringa:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Output:
```
3.1.2
```

Sostituisci le cifre che seguono 'fish' con 'X.X.X':

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Output:
```
Welcome to fish X.X.X
```

### Corrispondenze Avanzate con `awk`
Stampa la seconda colonna dei dati dove la prima colonna corrisponde a un modello specifico:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

Questo comando cerca righe in `datafile` dove la prima colonna inizia con una "a" seguita da una o più cifre e stampa la seconda colonna.

Integrando questi comandi esterni, i programmatori di Fish Shell possono sfruttare a pieno il potere delle espressioni regolari per compiti complessi di manipolazione del testo, migliorando le capacità native del shell.
