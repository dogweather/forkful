---
title:    "Bash: Trovare la lunghezza di una stringa"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è una delle attività più comuni nel mondo della programmazione. È utile per molteplici motivi, ad esempio nella validazione dei dati inseriti dagli utenti o nella manipolazione di stringhe per la creazione di output personalizzati.

## Come fare

Ci sono diversi modi per trovare la lunghezza di una stringa in Bash, ma il metodo più comune è utilizzare il comando "expr" in combinazione con l'opzione "- length". Ecco un esempio:

```
stringa = "Ciao, mondo!"
lunghezza = `expr length "$ stringa"`
echo "$ lunghezza"
```

Questo codice restituirà il valore "13" poiché ci sono esattamente 13 caratteri nella stringa "Ciao, mondo!". È importante notare che il valore della variabile "lunghezza" deve essere racchiuso tra apici invertiti ("`` `") per essere eseguito correttamente.

Un altro metodo per trovare la lunghezza della stringa è utilizzare la lunghezza incorporata nel comando "echo". Ad esempio:

```
stringa = "Ciao, mondo!"
lunghezza = `echo -n "$ stringa" | wc -c`
echo "$ lunghezza"
```

Anche in questo caso, il valore della variabile "lunghezza" sarà "13". Tuttavia, in questo approccio, è necessario aggiungere l'opzione "-n" a "echo" per evitare di contare anche il carattere di nuova riga.

## Approfondimento

Sebbene i due metodi sopra descritti siano i più comuni, ci sono molte altre opzioni per trovare la lunghezza di una stringa in Bash. Ad esempio, è possibile utilizzare il comando "awk" o il knerning "wc -m". Inoltre, è importante tenere presente che in Bash ogni carattere ha una lunghezza di un byte, quindi se si lavora con stringhe multibyte (ad esempio in UTF-8), è necessario utilizzare una logica più complessa per ottenere la lunghezza corretta.

## Vedi anche
- [Documentazione di Bash](https://www.gnu.org/software/bash/manual/html_node/String-Manipulation.html)
- [Pagina di manuali di GNU expr](https://www.gnu.org/software/gnulib/manual/html_node/expr-invocation.html)
- [Guida ai caratteri multibyte in Bash](https://www.joeldare.com/wiki/tutorial:multibyte_characters_in_bash)