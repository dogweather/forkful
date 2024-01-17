---
title:                "Estrazione di sottostringhe"
html_title:           "PowerShell: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Estrazione di sottostringhe è il processo di ottenere una porzione specifica di una stringa più grande. I programmatori spesso usano questa tecnica per manipolare e gestire grandi quantità di dati in modo più efficiente.

## Come fare:

```PowerShell
$stringa = 'Questo è un esempio di una stringa'
$stringa.Substring(0, 6)
```

Output: "Questo è"

```PowerShell
$stringa.Split(' ')[0]
```

Output: "Questo"

```PowerShell
$stringa -split ' ' | Select-Object -First 3
```

Output: "Questo", "è", "un"

## Approfondimento:

L'esigenza di estrarre sottostringhe è nata con l'avvento dei primi linguaggi di programmazione, in cui non era possibile manipolare singoli caratteri o porzioni di stringhe. Oggi esistono diverse alternative come l'utilizzo di espressioni regolari o la funzione "Substring" disponibile in molti linguaggi di programmazione.

L'implementazione di questa tecnica dipende dal linguaggio utilizzato ma la logica di base è sempre la stessa: specificare l'indice di inizio e la lunghezza della sottostringa desiderata. In PowerShell, si può anche utilizzare il metodo "Split" per dividere una stringa in un array di sottostringhe in base a un separatore specificato.

## Vedi anche:

Per maggiori informazioni sull'estrazione di sottostringhe, consulta gli articoli di [Microsoft Docs](https://docs.microsoft.com/it-it/powershell/scripting/getting-started/fundamental/working-with-strings?view=powershell-7.1) e [GeeksforGeeks](https://www.geeksforgeeks.org/powershell-split-strings-into-an-array/).