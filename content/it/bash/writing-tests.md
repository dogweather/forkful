---
title:    "Bash: Scrivere test"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Perché

Scrivere test è un'attività fondamentale per garantire la qualità del codice e ridurre gli errori. I test permettono di verificare il corretto funzionamento delle parti di un programma e di individuare eventuali problemi in modo rapido ed efficiente.

## Come fare

Per scrivere test in Bash, è possibile utilizzare il comando `test` o la sintassi `[[...]]`. Ecco un esempio di un semplice test utilizzando il comando `test`:

```
#!/bin/bash

# Dichiarazione di due variabili
x=5
y=8

# Test sulla variabile x: è uguale a 5?
if test $x -eq 5; then
  echo "La variabile x è uguale a 5"
fi

# Test sulla variabile y: è diversa da 10?
if test $y -ne 10; then
  echo "La variabile y è diversa da 10"
fi
```

Ecco l'output del codice sopra riportato:

```
La variabile x è uguale a 5
La variabile y è diversa da 10
```

Un altro modo per scrivere test in Bash è utilizzare la sintassi `[[...]]`, che offre più opzioni di confronto e supporta anche le espressioni regolari. Ecco un esempio utilizzando `[[...]]`:

```
#!/bin/bash

# Dichiarazione di una variabile
stringa="Questo è un esempio di test stringa"

# Test sulla variabile stringa: contiene la parola "esempio"?
if [[ "$stringa" =~ "esempio" ]]; then
  echo "La stringa contiene la parola 'esempio'"
fi
```

Output:

```
La stringa contiene la parola 'esempio'
```

## Approfondimento

Per scrivere test più avanzati in Bash, è possibile utilizzare la libreria `shunit2`, che permette di scrivere test automatizzati con una struttura più complessa. Questo è utile soprattutto nei progetti più grandi e complessi, dove è necessario testare molteplici funzionalità del codice.

È inoltre importante tenere in considerazione l'approccio alla scrittura dei test in Bash in modo corretto, seguendo le buone pratiche di sviluppo. Ad esempio, è necessario scegliere i giusti punti di controllo e implementare i corretti meccanismi di gestione degli errori.

## Vedi anche

- [Sintassi di base di Bash](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- [Guida alla scrittura di test in Bash](https://www.ibm.com/developerworks/library/l-bash-test/)
- [Libreria Shunit2 per test automatizzati](https://github.com/kward/shunit2)