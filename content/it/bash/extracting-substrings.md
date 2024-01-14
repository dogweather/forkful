---
title:    "Bash: Estrazione di sottostringhe."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è una parte importante della programmazione Bash perché può aiutare a manipolare e gestire i dati in modo più efficiente. È particolarmente utile quando si lavora con grandi quantità di testo o file, rendendo il processo più veloce ed efficace.

## Come Fare

L'estrazione di sottostringhe in Bash è un processo semplice che richiede l'utilizzo del comando `echo` e di alcuni caratteri speciali.

Ecco un esempio di come estrarre una sottostringa da una variabile:

```Bash
frase="Benvenuto nel mio blog post!"
echo ${frase:9:4} 
```

Questa sintassi utilizza i due punti per indicare l'inizio e la lunghezza della sottostringa che vogliamo estrarre. In questo esempio, estraiamo i caratteri dalla posizione 9 per una lunghezza di 4, quindi l'output sarà "nel mio".

Se invece vogliamo estrarre una sottostringa a partire dalla fine della variabile, possiamo utilizzare una sintassi simile:

```Bash
echo ${frase: -8} 
```

In questo caso, il segno meno indica l'estrazione dalla fine e l'output sarà "blog post!".

## Approfondimento

Oltre alla semplice estrazione di una sottostringa da una variabile, il comando `sed` può essere utilizzato per estrarre sottostringhe da file di testo.

Ad esempio, se vogliamo estrarre una sottostringa da un file di testo basata su un modello di ricerca, possiamo utilizzare questo comando:

```Bash
sed -n '/modello/p' file.txt
```

Questo restituirà tutte le righe del file che contengono il modello cercato.

Inoltre, possiamo anche utilizzare `grep` per estrarre sottostringhe basate su un modello di ricerca, ad esempio:

```Bash
grep modello file.txt
```

Questo restituirà tutte le righe del file che contengono il modello cercato.

## Vedi Anche

- [Guida alla formattazione dei testi in Bash](https://www.linuxnix.com/text-formatting-in-bash/)
- [Tutorial di estrazione di sottostringhe in Bash](https://www.tecmint.com/extract-string-from-variable-shell-script/)
- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)