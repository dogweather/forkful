---
title:    "Bash: Concatenazione di stringhe"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione fondamentale nella programmazione Bash che consente di unire più stringhe in una sola. Questo è utile quando si vogliono creare messaggi o output dinamici che includono variabili o testo predefinito.

## Come fare

Per concatenare due o più stringhe in Bash, è necessario utilizzare il simbolo di "arco" (`$`) seguito dal nome della prima stringa, seguito dalla seconda stringa tra doppi apici (`" "`) e stringhe aggiuntive separate da spazi. Ad esempio:

```Bash
s1="Ciao"
s2="mondo"
echo "$s1 $s2!"  # Output: Ciao mondo!
```

Se si desidera aggiungere una stringa alla fine di una variabile esistente, è possibile utilizzare l'operatore di assegnamento `+=`. Ad esempio:

```Bash
var="Ciao"
var+=" mondo!"
echo "$var"  # Output: Ciao mondo!
```

È anche possibile concatenare stringhe all'interno di una variabile utilizzando le parentesi graffe `${}` e il simbolo di "arco". Ad esempio:

```Bash
var="Hello "
var2="World"
var3="${var} ${var2}!"  # Utilizzo delle parentesi graffe
echo "${var3}"  # Output: Hello World!
```

## Approfondimenti

La concatenazione di stringhe è una tecnica fondamentale nella programmazione Bash, ma è importante tenere presente alcuni punti:

- Quando si concatena una stringa a una variabile, è necessario utilizzare il simbolo di "arco" (`$`) per inserire il contenuto della variabile all'interno della nuova stringa.
- Se si sta utilizzando un comando all'interno di una concatenazione di stringhe, è necessario utilizzare gli apici "inversi" (``` `` ```) per eseguire il comando e inserirne il risultato nella stringa.

## Vedi anche

Per ulteriori informazioni sulla concatenazione di stringhe in Bash, consultare i seguenti link:

- [Concatenazione di stringhe in Bash su BashTutorial](https://www.baeldung.com/linux/concatenate-strings-bash)
- [Concatenazione di stringhe in Bash su GeeksforGeeks](https://www.geeksforgeeks.org/concatenate-strings-together-in-bash/)
- [Riferimento alla sintassi di Bash su GNU](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion)