---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:09:59.352003-07:00
description: "Gli array associativi sono come degli array super-potenziati che ti\
  \ permettono di usare stringhe come indici invece di semplici interi. I programmatori\
  \ li\u2026"
lastmod: '2024-02-25T18:49:41.453066-07:00'
model: gpt-4-0125-preview
summary: "Gli array associativi sono come degli array super-potenziati che ti permettono\
  \ di usare stringhe come indici invece di semplici interi. I programmatori li\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi sono come degli array super-potenziati che ti permettono di usare stringhe come indici invece di semplici interi. I programmatori li utilizzano per strutture dati più complesse, rendendo più semplice gestire dati che non si adattano ordinatamente in un elenco sequenziale.

## Come fare:

Prima di tutto, dichiara un array associativo in Bash:

```Bash
declare -A my_array
```

Quindi, puoi iniziare a popolarlo con valori, usando stringhe come chiavi:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programmazione"
```

Per accedere a un elemento, usa la sua chiave:

```Bash
echo ${my_array["name"]}  # Output: Linux Journal
```

Iterare sopra chiavi e valori è altrettanto semplice:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

Un output di esempio potrebbe apparire così:

```
name: Linux Journal
topic: Programmazione
```

Per aggiungere o modificare elementi, basta assegnare un valore a una chiave, in modo simile alla popolazione iniziale:

```Bash
my_array["readers"]="Voi"
```

E per rimuovere un elemento, usa `unset`:

```Bash
unset my_array["topic"]
```

## Approfondimento

Gli array associativi sono stati introdotti nella versione 4.0 di Bash, rendendoli un'aggiunta relativamente recente al linguaggio. Prima della loro introduzione, gestire array con indici non interi era laborioso, spesso richiedendo soluzioni alternative o strumenti esterni come `awk` o `sed`.

Sotto il cofano, Bash implementa gli array associativi usando tabelle hash. Questa implementazione permette una ricerca chiave efficiente, che rimane piuttosto costante a prescindere dalla dimensione dell'array, una caratteristica critica per la performance nell'esecuzione di script.

Sebbene gli array associativi in Bash portino molta potenza e flessibilità allo scripting di shell, vengono con il loro set di limitazioni, come essere in qualche modo più ingombranti da utilizzare rispetto ad array in linguaggi di livello superiore come Python o JavaScript. Per compiti di manipolazione dati complessi, potrebbe comunque valer la pena considerare strumenti esterni o linguaggi più adatti al compito.

Tuttavia, per molte attività di scripting tipiche, gli array associativi forniscono uno strumento prezioso nel kit del programmatore Bash, permettendo script più leggibili e mantenibili consentendo l'uso di chiavi stringa significative invece di indici numerici.
