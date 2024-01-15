---
title:                "Capitalizzare una stringa"
html_title:           "Fish Shell: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Capitalize è una funzione utile per ottenere la versione maiuscola di una stringa, che può essere utile in diversi casi, come l'ordinamento di elenchi o la formattazione di testi.

## Come
```Fish Shell
set text "ciao mondo"
echo (string capitalize $text)
```

Output: Ciao mondo

Se si vuole capitalizzare solo la prima lettera di una stringa, si può utilizzare la funzione `string upper`:

```Fish Shell
set text "ciao mondo"
echo (string upper --first $text)
```

Output: Ciao mondo

## Deep Dive
La funzione `capitalize` è implementata nel modulo `string` di Fish Shell. Esaminiamo come funziona all'interno del codice sorgente:

```C
void string_capitalize(const wchar_t *text) {
    while (*text) {
        fwprintf(stderr, L"%lc", towupper(*text));
        text++;
    }
}

// Codice sorgente abbreviato

function -d -t string capitalize --description "Capitalizza una stringa.<br />In caso contrario restituisce una stringa vuota." --argument summary --namespace string uppercase --no-scope --terse uppercase ->
    if test (count $argv) -gt 0
        for text in $argv
            string capitalize $text
        end
    else
        while read -l line
            string capitalize $line
        end
    end
end
```

La funzione si basa su una semplice iterazione sulle lettere della stringa, utilizzando la funzione standard `towupper` per ottenere la versione maiuscola di ogni carattere. Invece, il codice della funzione `uppercase` sfrutta la funzione `capitalize` interna di Fish Shell per gestire tutti i casi possibili di input.

## Vedi anche
- Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/
- Tutorial su Fish Shell: https://www.linux.it/~ott/al_colophon/tutorial/fish.html
- Lista di funzioni di stringhe di Fish Shell: https://fishshell.com/docs/current/cmds/string.html