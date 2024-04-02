---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:52.472331-07:00
description: "Gli array associativi, o mappe hash, permettono di memorizzare dati\
  \ come coppie chiave-valore, rendendo pi\xF9 semplice organizzare e recuperare informazioni\u2026"
lastmod: '2024-03-13T22:44:43.849235-06:00'
model: gpt-4-0125-preview
summary: "Gli array associativi, o mappe hash, permettono di memorizzare dati come\
  \ coppie chiave-valore, rendendo pi\xF9 semplice organizzare e recuperare informazioni\u2026"
title: Utilizzo di array associativi
weight: 15
---

## Cos'è e Perché?

Gli array associativi, o mappe hash, permettono di memorizzare dati come coppie chiave-valore, rendendo più semplice organizzare e recuperare informazioni mediante la chiave. Sono utili quando si necessita di un modo più strutturato di gestire i dati rispetto alle semplici liste, specialmente nelle configurazioni e quando si ha a che fare con un insieme di attributi.

## Come fare:

Fish non supporta nativamente gli array associativi come Bash 4+, ma è possibile ottenere una funzionalità simile utilizzando una combinazione di liste e manipolazione di stringhe. Ecco come emularli:

Prima, configurando separatamente gli elementi "array associativo":

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

Per accedere a un elemento, basta fare riferimento direttamente ad esso:

```Fish Shell
echo $food_color_apple
# Output: red
```

Se è necessario iterarli, usare un ciclo for tenendo conto di una convenzione di denominazione:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Output:
# red
# yellow
```

Per coloro che sentono la mancanza del `${!array[@]}` di Bash per ottenere tutte le chiavi, è possibile memorizzare le chiavi in una lista separata:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'is' $food_color_$key
end
# Output:
# apple is red
# banana is yellow
```

## Approfondimento

Array associativi veri e propri, come in altri linguaggi di scripting, non fanno ancora parte dell'approccio di Fish. Il trucco mostrato sfrutta le capacità di manipolazione delle stringhe e delle liste di Fish per creare una struttura pseudo-array associativa. Anche se funziona, non è pulito o privo di errori come lo sarebbe il supporto incorporato per gli array associativi. Altri shell come Bash e Zsh forniscono funzionalità incorporate di array associativi, che risultano in codice più diretto e leggibile. Tuttavia, la filosofia di design di Fish mira alla semplicità e alla facilità d'uso, possibilmente a scapito di tali funzionalità. La soluzione soddisfa la maggior parte delle esigenze, ma tenete d'occhio l'evoluzione di Fish Shell: i suoi sviluppatori migliorano attivamente e aggiungono funzionalità basate sui feedback della comunità.
