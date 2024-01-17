---
title:                "Scrivere un file di testo"
html_title:           "Python: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

Cosa e Perché?
Scrivere un file di testo in Python è un modo comune per salvare informazioni in un formato leggibile dalle macchine. I programmatori lo fanno per creare dati persistenti che possono essere manipolati, analizzati o utilizzati in futuro.

Come Fare:
Di seguito un esempio del codice Python per creare e scrivere in un file di testo chiamato "mionome.txt":

```Python
nome = "Giulia"
cognome = "Rossi"
eta = 25
with open("mionome.txt", "w") as file:
    file.write("Nome: " + nome + "\n")
    file.write("Cognome: " + cognome + "\n")
    file.write("Età: " + str(eta))
```

L'output del codice sarà un file di testo con il seguente contenuto:
Nome: Giulia
Cognome: Rossi
Età: 25

Deep Dive:
La scrittura di un file di testo è stata una funzionalità introdotta in Python 1.1 nel 1994. Oggi, ci sono molte alternative per salvare dati in un formato leggibile, come l'utilizzo di database o di formati più strutturati come JSON. Tuttavia, la scrittura di un file di testo è ancora utile per scopi semplici o per mantenere la compatibilità con sistemi più vecchi.

See Also:
- [Capitolo su File di Testo della documentazione ufficiale di Python] (https://docs.python.org/it/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Articolo su file di testo su programmazione facile per principianti] (https://www.programmareinpython.it/cosa-sono-e-a-cosa-servono-i-file-di-testo/)
- [Tutorial su file di testo e database in Python] (https://www.digitalocean.com/community/tutorials/how-to-manage-files-and-directories-in-python-3)