---
title:    "Bash: Estrazione di sottostringhe"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con il linguaggio di programmazione Bash, potresti avere la necessità di estrarre delle sottostringhe da una stringa più grande. Ciò può essere necessario per lavorare con dati formattati in modo specifico o per svolgere varie attività di manipolazione dei dati. In questa guida, esploreremo come estrarre delle sottostringhe in Bash e perché potrebbe essere utile farlo.

## Come Fare

Per estrarre delle sottostringhe in Bash, è possibile utilizzare il comando `cut`. Questo comando può essere utilizzato per estrarre porzioni specifiche di una stringa, basandosi su un delimitatore specifico. Ad esempio, se avessimo una stringa formattata come "Nome_Cognome", possiamo utilizzare il seguente comando per estrarre solo il nome:

```Bash
stringa="Mario_Rossi"
echo $stringa | cut -d _ -f 1
```

In questo caso, il delimitatore specificato è l'underscore (_) e l'opzione `-f` indica quale campo della stringa estrarre.

Possiamo anche utilizzare il comando `grep` per estrarre delle sottostringhe in base a un determinato pattern. Ad esempio, se avessimo una stringa formattata come "Nome_Comune_Eta", possiamo utilizzare il seguente comando per estrarre solo l'età:

```Bash
stringa="Mario_Roma_30"
echo $stringa | grep -o '[0-9]*$'
```

In questo caso, il pattern specificato con l'opzione `-o` indica di estrarre solo i numeri alla fine della stringa.

## Approfondimento

Ci sono molte altre opzioni e utilizzazioni possibili per estrarre delle sottostringhe in Bash, come l'utilizzo di espressioni regolari o l'estrazione di sottosequenze di caratteri. Inoltre, è possibile utilizzare i comandi `sed` e `awk` per svolgere compiti simili. Sperimenta con questi comandi e vedi quali funzionano meglio per le tue esigenze.

## Vedi Anche

- [Documentazione di Bash su `cut`](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
- [Maggiori informazioni su `grep`](https://www.gnu.org/software/grep/manual/grep.html)
- [Tutorial su `sed`](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)
- [Introduzione ad `awk`](https://www.digitalocean.com/community/tutorials/how-to-use-the-awk-language-to-manipulate-text-in-linux)