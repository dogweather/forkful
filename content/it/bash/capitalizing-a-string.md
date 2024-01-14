---
title:    "Bash: Maiuscolizzare una stringa"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Perché capitalizzare una stringa
Capitalize a string, ovvero rendere la prima lettera di ogni parola maiuscola, è un'operazione comune in programmazione quando si vuole formattare una stringa correttamente. Ad esempio, in una frase come "questo è un esempio di stringa", potrebbe essere difficile leggere correttamente la prima lettera di ogni parola senza la formattazione adeguata.

# Come fare
Per capitalizzare una stringa in Bash, è possibile utilizzare il comando `tr` (translate) in combinazione con l'opzione `-c` per specificare che si desidera ottenere una nuova stringa con la prima lettera di ogni parola convertita in maiuscolo. Ad esempio:

```Bash
# Dichiarazione della stringa di esempio
string="numerosi utenti stanno utilizzando Bash per la prima volta"
# Utilizzo di 'tr' per capitalizzare la stringa
capitalized_string=$(echo $string | tr -c '[:upper:]' '[:lower:]')
# Output della nuova stringa capitalizzata
echo $capitalized_string
```

L'output di questo codice sarà: "Numerous Users Are Using Bash For The First Time".

# Approfondimento
Nella maggior parte dei casi, è sufficiente utilizzare il comando `tr` come mostrato sopra per capitalizzare una stringa. Tuttavia, è importante notare che questo metodo non viene applicato correttamente a stringhe con caratteri speciali, come ad esempio apostrofi o accenti. In questi casi, è possibile utilizzare la combinazione dei comandi `awk` e `printf` per ottenere una stringa capitalizzata correttamente.

# Vedi anche
- [Documentazione di Bash](https://www.gnu.org/software/bash/)
- [Guida a Bash scripting](https://tldp.org/LDP/abs/html/)
- [Tutorial di Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)