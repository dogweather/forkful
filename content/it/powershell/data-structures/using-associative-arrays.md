---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:32.706927-07:00
description: "Gli array associativi, noti anche come tabelle hash o dizionari in PowerShell,\
  \ consentono di memorizzare i dati in coppie chiave-valore, rendendo il\u2026"
lastmod: '2024-03-13T22:44:43.633975-06:00'
model: gpt-4-0125-preview
summary: "Gli array associativi, noti anche come tabelle hash o dizionari in PowerShell,\
  \ consentono di memorizzare i dati in coppie chiave-valore, rendendo il\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi, noti anche come tabelle hash o dizionari in PowerShell, consentono di memorizzare i dati in coppie chiave-valore, rendendo il recupero dei dati semplice ed efficiente. I programmatori li utilizzano per immagazzinare dati correlati insieme in un modo che è facile da accedere tramite chiave.

## Come fare:

Creare e utilizzare array associativi in PowerShell è abbastanza semplice. Ecco come si fa la magia:

**Creare un array associativo:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Ingegnere"
```

Questo frammento di codice crea un array associativo con tre coppie chiave-valore.

**Accedere ai valori:**

Per ottenere un valore, fare riferimento alla sua chiave:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**Output di esempio:**

```
Alex
```

**Aggiungere o modificare dati:**

Basta usare la chiave per aggiungere una nuova coppia o modificare una esistente:

```PowerShell
$myAssociativeArray["location"] = "New York" # Aggiunge una nuova coppia chiave-valore
$myAssociativeArray["job"] = "Ingegnere Senior" # Modifica una coppia esistente
```

**Iterare su un array associativo:**

Itera tra chiavi e valori così:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**Output di esempio:**

```
name : Alex
age : 25
job : Ingegnere Senior
location : New York
```

## Approfondimento

Il concetto di array associativi è comune in molti linguaggi di programmazione, solitamente denominato dizionario, mappa o tabella hash a seconda del linguaggio. In PowerShell, gli array associativi sono implementati come tabelle hash, che sono molto efficienti per la ricerca di chiavi, lo stoccaggio di dati e il mantenimento di una collezione di chiavi uniche.

Storicamente, gli array associativi forniscono un mezzo per gestire collezioni di oggetti in cui ogni elemento può essere rapidamente recuperato senza dover iterare attraverso l'intera collezione, utilizzando la sua chiave. L'efficienza nel recupero e nella modifica dei dati negli array associativi li rende una scelta preferita per vari compiti. Tuttavia, hanno limitazioni, come il mantenimento dell'ordine, per cui i dizionari ordinati o gli oggetti personalizzati potrebbero essere un'alternativa migliore.

Nonostante le loro limitazioni, gli array associativi/tabelle hash in PowerShell sono incredibilmente flessibili e uno strumento potente per la scrittura di script. Consentono uno stoccaggio dinamico dei dati e sono particolarmente utili nelle configurazioni, nella manipolazione dei dati e ovunque sia necessario un formato di dati strutturato senza l'onere di una definizione formale di classe. Ricorda solo che, mentre gli array associativi sono perfetti per il recupero basato su chiavi, se il tuo compito coinvolge strutture dati complesse o richiede il mantenimento di un ordine specifico, potresti voler esplorare altri tipi di dati o oggetti personalizzati all'interno di PowerShell.
