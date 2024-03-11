---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:20.512561-07:00
description: "Gli array associativi, spesso noti come dizionari in Visual Basic for\
  \ Applications (VBA), consentono ai programmatori di creare collezioni di coppie\u2026"
lastmod: '2024-03-11T00:14:16.821589-06:00'
model: gpt-4-0125-preview
summary: "Gli array associativi, spesso noti come dizionari in Visual Basic for Applications\
  \ (VBA), consentono ai programmatori di creare collezioni di coppie\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cos'è e perché?

Gli array associativi, spesso noti come dizionari in Visual Basic for Applications (VBA), consentono ai programmatori di creare collezioni di coppie chiave-valore. Questa funzionalità è fondamentale per un'efficace memorizzazione e recupero dei dati, offrendo un modo più flessibile e intuitivo di gestire i dati rispetto agli indici degli array tradizionali.

## Come fare:

In VBA, l'oggetto `Dictionary` fornisce funzionalità simili agli array associativi. Devi prima aggiungere un riferimento al Microsoft Scripting Runtime per usarlo:

1. Nell'editor VBA, vai su Strumenti > Riferimenti...
2. Seleziona "Microsoft Scripting Runtime" e clicca su OK.

Ecco come dichiarare, popolare e accedere agli elementi in un `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Aggiungere elementi
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' Accedere agli elementi
Debug.Print sampleDictionary.Item("Name")  ' Risultato: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Risultato: 29

' Verificare se una chiave esiste
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Esiste la chiave Occupation"
End If

' Rimuovere elementi
sampleDictionary.Remove("Occupation")

' Ciclare attraverso il dizionario
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Approfondimento

L'oggetto `Dictionary` si interfaccia, sottotraccia, con componenti del Windows Scripting Host. Come tale, è un oggetto COM con binding tardivo, che era un modo comune per estendere la funzionalità di VBA in passato. Il suo utilizzo in VBA può migliorare significativamente la capacità del linguaggio di manipolare set di dati complessi senza imporre una struttura rigida, come si vede negli array tradizionali o negli intervalli di Excel.

Una limitazione da tenere presente è che l'accesso al `Dictionary` richiede l'impostazione di un riferimento al Microsoft Scripting Runtime, ciò può complicare la distribuzione dei tuoi progetti VBA. Esistono alternative come le Collezioni all'interno di VBA ma mancano di alcune delle caratteristiche chiave del `Dictionary`, come la possibilità di controllare facilmente l'esistenza di una chiave senza innescare un errore.

In contesti di programmazione più recenti, linguaggi come Python offrono supporto integrato per gli array associativi (noti anche come dizionari in Python) senza la necessità di aggiungere riferimenti esterni. Questo supporto integrato semplifica il processo e offre funzionalità più avanzate già pronte all'uso. Tuttavia, nei limiti di VBA e per applicazioni specifiche orientate all'automazione di compiti nella suite Microsoft Office, l'utilizzo dell'oggetto `Dictionary` rimane un metodo potente e pertinente per strutture dati simil-array associative.
