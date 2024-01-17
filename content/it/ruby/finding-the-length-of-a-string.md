---
title:                "Trova la lunghezza di una stringa"
html_title:           "Ruby: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Cos'è e perché?
Trovare la lunghezza di una stringa è una delle operazioni fondamentali nel mondo della programmazione. Essenzialmente, ci permette di sapere quanti caratteri compongono una certa stringa di testo. Questo è utile per molti motivi, ad esempio per controllare che una stringa non sia vuota o per gestire la formattazione di un output.

# Come:
``` Ruby
stringa = "Ciao a tutti!"
puts stringa.length
```
L'output sarà `13`, ovvero il numero di caratteri della stringa "Ciao a tutti!".

# Approfondimenti:
Trovare la lunghezza di una stringa è un'operazione molto comune, ma può essere eseguita in modi diversi a seconda del linguaggio di programmazione utilizzato. Ad esempio, in Ruby si può utilizzare il metodo `.length` come visto nell'esempio sopra, ma in altri linguaggi come Java si può usare il metodo `.length()` o `.size()`. Inoltre, è importante notare che alcuni linguaggi possono considerare anche gli spazi vuoti o altri caratteri speciali come parte della lunghezza della stringa, mentre in Ruby si contano solo i caratteri effettivi.

# Vedi anche:
- Documentazione ufficiale su Ruby String:
https://ruby-doc.org/core-2.7.3/String.html
- Altri metodi per trovare la lunghezza di una stringa in altri linguaggi:
https://www.w3schools.com/jsref/jsref_length_string.asp