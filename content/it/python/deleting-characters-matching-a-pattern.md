---
title:    "Python: Cancellazione dei caratteri corrispondenti a un modello"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Perché
La cancellazione di caratteri che corrispondono ad un determinato pattern può essere utile per pulire e gestire grandi quantità di dati in modo efficiente.

# Come Fare
Di seguito è riportato un esempio di codice Python che mostra come eliminare i caratteri che corrispondono ad un determinato pattern utilizzando il modulo "re" per le espressioni regolari:

```
import re

# Definiamo il nostro testo di esempio
testo = "Questo è un testo di esempio con alcune parole che vogliamo eliminare #hashtag"

# Definiamo il pattern che corrisponde ai nostri hashtag
pattern = r"#\w+"

# Utilizziamo la funzione "sub" per sostituire il pattern con una stringa vuota (ovvero eliminarlo)
nuovo_testo = re.sub(pattern, "", testo)

# Stampiamo il nuovo testo senza i hashtag
print(nuovo_testo)

```

Output:
"Questo è un testo di esempio con alcune parole che vogliamo eliminare"

In this example, we use the ```sub()``` function from the "re" module to replace the pattern with an empty string, effectively deleting it from the original text. This allows for quick and efficient cleanup of data.

# Approfondimento
La cancellazione di caratteri che corrispondono ad un pattern può essere fatta utilizzando anche altre funzioni del modulo "re", come ad esempio ```findall()``` per trovare tutte le occorrenze del pattern nel testo e ```split()``` per dividere il testo in base al pattern specificato.

# Vedi Anche
- Documentazione ufficiale del modulo "re": https://docs.python.org/3/library/re.html
- Tutorial su espressioni regolari in Python: https://www.programiz.com/python-programming/regex