---
title:                "Scrivere un file di testo"
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Scrivere un file di testo in Python permette di salvare dati in modo permanente. I programmatori lo fanno per tenere traccia dei risultati, configurare programmi, o esportare dati per altri usi.

## How to: (Come fare:)
Ecco come creare e scrivere su un file di testo:

```Python
# Creare e aprire un file in modalità scrittura ('w' sta per 'write')
with open('esempio.txt', 'w') as file:
    file.write("Ciao, mondo!")

# Aggiungere altre righe al file
with open('esempio.txt', 'a') as file:  # 'a' sta per 'append'
    file.write("\nAggiungo una nuova riga.")
```

Il contenuto del file `esempio.txt` sarà:

```
Ciao, mondo!
Aggiungo una nuova riga.
```

## Deep Dive (Approfondimento)
Nel corso degli anni, il modo di scrivere su file è cambiato poco, principalmente per mantenere la semplicità. Oltre al metodo `write()`, esiste `writelines()` per scrivere una lista di stringhe. Un dettaglio importante è la gestione delle eccezioni e l'uso di encoding corretto (tipicamente `'utf-8'`). In alternativa, moduli come `csv` e `json` possono essere usati per scrivere in formati specifici.

## See Also (Vedi anche)
Per approfondire, ecco alcuni link utili:

- Documentazione ufficiale Python su I/O di file: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Gestione delle eccezioni in Python: https://docs.python.org/3/tutorial/errors.html
- Modulo CSV in Python: https://docs.python.org/3/library/csv.html
- Modulo JSON in Python: https://docs.python.org/3/library/json.html
