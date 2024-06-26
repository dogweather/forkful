---
date: 2024-01-20 17:34:11.132443-07:00
description: 'How to: Per concatenare stringhe in Bash basta metterle vicine. Senza
  spazi, senza niente.'
lastmod: '2024-03-13T22:44:43.589765-06:00'
model: gpt-4-1106-preview
summary: Per concatenare stringhe in Bash basta metterle vicine.
title: Concatenazione di stringhe
weight: 3
---

## How to:
Per concatenare stringhe in Bash basta metterle vicine. Senza spazi, senza niente.

```Bash
#!/bin/bash

# Concatenazione semplice
stringa1="Ciao, "
stringa2="come stai?"
saluto=$stringa1$stringa2
echo $saluto # Output: Ciao, come stai?

# Con variabili e testo diretto
nome="Mario"
echo "Buongiorno, "$nome"!" # Output: Buongiorno, Mario!

# Usando le parentesi graffe per chiarezza
messaggio="Ehi tu, ${nome}, sì proprio tu!"
echo $messaggio # Output: Ehi tu, Mario, sì proprio tu!
```

## Deep Dive
Concatenare è fondamentale da quando è nato lo scripting: è un'operazione di base. È possibile anche usare `echo`, `printf`, o concatenare durante l'assegnazione di una variabile.

La sintassi `${variabile}` serve a delimitare il nome di una variabile da ciò che segue, per evitare confusione. Questo è particolarmente utile se si vogliono aggiungere caratteri subito dopo il valore della variabile, come lettere o numeri.

Un altro metodo è usare il comando `paste` per concatenare il contenuto di file, ma va oltre l'ambito di una semplice stringa.

## See Also
- Bash String Manipulation Guide: https://tldp.org/LDP/abs/html/string-manipulation.html
- Advanced Bash Scripting Guide: https://tldp.org/LDP/abs/html/
- Stack Overflow: https://stackoverflow.com/questions/tagged/bash+string-concatenation
