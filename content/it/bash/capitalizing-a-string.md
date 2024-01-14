---
title:    "Bash: Capitalizzare una stringa"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché
Il capitalizzare una stringa è un'operazione molto comune nella programmazione, specialmente quando si lavora con dati sensibili come password o informazioni personali. Capitalizzare una stringa significa trasformare tutte le lettere in maiuscolo, fornendo una maggiore sicurezza e leggibilità del codice.

## Come fare
Per capitalizzare una stringa in Bash, possiamo utilizzare il comando `tr` e il costrutto `echo` per catturare l'input dall'utente. Di seguito un esempio di codice che richiede all'utente di inserire una stringa e la trasforma in maiuscolo:

```Bash
echo "Inserisci una stringa:"
read stringa

stringa_cap=$(echo "$stringa" | tr '[:lower:]' '[:upper:]')

echo "La stringa in maiuscolo è: $stringa_cap"
```

L'output di questo codice sarà qualcosa del tipo:

```
Inserisci una stringa:
ciao a tutti
La stringa in maiuscolo è: CIAO A TUTTI
```

## Approfondimento
Esistono diverse tecniche per capitalizzare una stringa in Bash, ma l'utilizzo del comando `tr` è uno dei più semplici ed efficaci. Tuttavia, è importante notare che la trasformazione in maiuscolo viene effettuata solo sui caratteri ASCII, quindi se la stringa contiene caratteri non ASCII, questi rimarranno invariati.

Un altro modo per capitalizzare una stringa è utilizzare il costrutto `sed` in combinazione con le espressioni regolari. Inoltre, possiamo anche creare una funzione personalizzata che ci permetta di capitalizzare una stringa secondo le nostre esigenze.

In generale, il capitalizzare una stringa in Bash ci permette di avere un maggiore controllo sui dati con cui lavoriamo e garantisce una maggiore sicurezza nelle nostre applicazioni.

## Vedi anche
- [Documentazione su `tr`](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Tutorial su espressioni regolari in Bash](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-regex-in-linux-commands)
- [Esempio di funzione per capitalizzare una stringa in Bash](https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash)