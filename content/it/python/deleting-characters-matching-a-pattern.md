---
title:                "Python: Eliminare i caratteri corrispondenti ad un modello."
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché
Capita spesso che nel processo di sviluppo di un programma si debbano eliminare caratteri corrispondenti ad uno specifico schema. Questa operazione può essere utile per pulire ed elaborare input grezzi, oppure per rimuovere informazioni superflue da una stringa.

## Come Fare
In Python, è possibile utilizzare il metodo ```replace()``` per eliminare caratteri specifici da una stringa basandosi su un pattern. Di seguito un esempio di codice e il relativo output:

```Python
stringa = "Questa è una stringa con *caratteri* da eliminare."
carattere_da_eliminare = "*"

# Usa il metodo replace() per eliminare il carattere dalla stringa
nuova_stringa = stringa.replace(carattere_da_eliminare, "")

print(nuova_stringa)

# Output: "Questa è una stringa con caratteri da eliminare."
```

In questo esempio, il metodo ```replace()``` sostituisce il carattere ```*``` con una stringa vuota, eliminandolo dalla stringa originale.

## Approfondimento
Esistono diverse situazioni in cui può essere utile eliminare caratteri corrispondenti ad uno specifico pattern. Ad esempio, se si sta analizzando un file di testo con dati grezzi, si può utilizzare questa tecnica per pulire l'input e rendere i dati più leggibili per il programma.

Inoltre, si può anche utilizzare il metodo ```replace()``` per eliminare più caratteri in una sola volta, specificando una lista di caratteri da sostituire. Per esempio:

```Python
stringa = "Questa è una stringa con [caratteri] da #eliminare."
caratteri_da_eliminare = ["[", "]", "#"]

# Usa il metodo replace() per eliminare i caratteri dalla stringa
nuova_stringa = stringa.replace(caratteri_da_eliminare, "")

print(nuova_stringa)

# Output: "Questa è una stringa con caratteri da eliminare."
```

Infine, è importante ricordare che il metodo ```replace()``` ritorna una nuova stringa e non modifica la stringa originale. Per modificare direttamente la stringa originale, è possibile utilizzare il metodo ```strip()```.

## Vedi Anche
- [Python string methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Python replace() method documentation](https://docs.python.org/3/library/stdtypes.html#str.replace)