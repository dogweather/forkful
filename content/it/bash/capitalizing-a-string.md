---
title:                "Bash: Capitalizzare una stringa"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché capitalizzare una stringa in Bash

La capitalizzazione di una stringa in Bash è un'operazione comune che può essere utile in molte situazioni diverse. Ad esempio, si potrebbe voler visualizzare una stringa in maiuscolo per renderla più visibile o per confrontarla con altre stringhe in modo più preciso.

## Come capitalizzare una stringa in Bash

Per capitalizzare una stringa in Bash, è possibile utilizzare il comando "tr" insieme all'opzione "-u", che converte tutti i caratteri in maiuscolo. Ad esempio, per capitalizzare una stringa di input chiamata "stringa", si può utilizzare il seguente comando:

```Bash
echo "stringa" | tr '[:lower:]' '[:upper:]'
```

Questo comando prende la stringa "stringa" come input, la converte in maiuscolo e la restituisce come output. Il risultato sarà "STRINGA".

## Approfondimenti sulla capitalizzazione di una stringa

Mentre il comando "tr" è utile per capitalizzare una stringa in Bash, ci sono alternative che possono essere più adatte a determinati casi d'uso. Ad esempio, se si vuole capitalizzare solo la prima lettera di una stringa, si può utilizzare il comando "sed" insieme all'opzione "s/\b\w/\u&/g". Questo comando sostituirà solo la prima lettera di ogni parola con la versione maiuscola.

```Bash
echo "ciao mondo" | sed 's/\b\w/\u&/g'
```

Il risultato sarà "Ciao Mondo".

Alcuni altri modi per capitalizzare una stringa in Bash includono l'utilizzo di espressioni regolari con il comando "awk" o l'utilizzo della funzione di shell "capitalize_first" in linguaggio di programmazione e scripting Bash.

## Vedi anche

- [La documentazione del comando "tr"](https://www.gnu.org/software/make/manual/html_node/tr-Utility.html)
- [La documentazione del comando "sed"](https://www.gnu.org/software/sed/manual/sed.html)
- [La documentazione della funzione "capitalize_first"](https://tldp.org/LDP/abs/html/string-manipulation.html)