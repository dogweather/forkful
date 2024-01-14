---
title:    "Python: Ricerca e sostituzione di testo"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo è un'operazione comune nella programmazione Python. Questo può essere utile per modificare rapidamente grandi quantità di testo o per fare modifiche specifiche a parti di codice.

## Come fare

Per iniziare, è necessario importare il modulo "re" che contiene le funzioni per la ricerca e la sostituzione di testo. Una volta importato il modulo, è possibile utilizzare il metodo "sub" per sostituire il testo all'interno di una stringa. Ecco un esempio di codice che sostituisce la parola "cane" con "gatto" all'interno di una stringa:

```Python
import re

testo = "Il mio cane è adorabile"
nuovo_testo = re.sub("cane", "gatto", testo)
print(nuovo_testo)
```

Il codice produrrà l'output "Il mio gatto è adorabile". Si noti che il metodo "sub" sostituisce solo la prima occorrenza del testo all'interno della stringa.

## Approfondimento

Il metodo "sub" ha anche la possibilità di sostituire più occorrenze di un testo utilizzando il parametro opzionale "count". Se si vuole sostituire solo le prime due occorrenze di "cane" all'interno della stringa "Il mio cane è adorabile", è possibile utilizzare il seguente codice:

```Python
import re

testo = "Il mio cane è adorabile, ma il cane del vicino è fastidioso"
nuovo_testo = re.sub("cane", "gatto", testo, count=2)
print(nuovo_testo)
```

Il codice produrrà l'output "Il mio gatto è adorabile, ma il gatto del vicino è fastidioso". Potete anche utilizzare espressioni regolari per cercare e sostituire testo. Ad esempio, se si vuole sostituire tutte le parole che iniziano con la lettera "T" con la parola "tempo", è possibile utilizzare il seguente codice:

```Python
import re

testo = "Il T-rex è un dinosauro"
nuovo_testo = re.sub("T\w+", "tempo", testo)
print(nuovo_testo)
```

Il codice produrrà l'output "Il tempo è un tempo". Si noti che espressioni regolari più complesse possono richiedere una maggiore conoscenza e comprensione.

## Vedi anche

- Documentazione ufficiale del modulo "re": https://docs.python.org/3/library/re.html
- Tutorial su espressioni regolari: https://regexone.com/
- Esempi di utilizzo del metodo "sub": https://www.tutorialspoint.com/python/string_sub.htm