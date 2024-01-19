---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Leggere gli argomenti della riga di comando consiste nel prelevare i dati forniti all'esecutore di un programma da un terminale o da uno script. Questo permette ai programmatori di personalizzare le operazioni di un programma in base alle esigenze dell'utente.

## Come Fare?

In Fish Shell, gli argomenti della riga di comando vengono letti tramite la variabile speciale chiamata `argv`. Ecco un esempio:

```Fish Shell
# definiamo una funzione 'stampa_argomenti'
function stampa_argomenti
    for arg in $argv
        echo $arg
    end
end
```

Ora, se eseguiamo il comando `stampa_argomenti Ciao Mondo`, otterremo:

```Fish Shell
Ciao
Mondo
```

## Approfondimento

Fish Shell nasce come alternativa ai tradizionali shell come Bash, con un'interfaccia più user-friendly e una semplificazione della sintassi. Anche se la lettura degli argomenti della riga di comando è abbastanza simile a altri shell, Fish offre un modo più intuitivo e diretto attraverso l'uso della variabile `argv`.

Altre alternative a Fish, includono Bash o Zsh, collegati ai sistemi Unix tanto tradizionali quanto moderni. Hanno il vantaggio di essere ubiquitari su vari ambienti, anche se tendono a essere più verbosi e complicati da usare.

## Vedere Anche

Per ulteriori informazioni sulla programmazione in Fish Shell e su come leggere gli argomenti della riga di comando, ecco alcuni link utili:

- [Manuale Ufficiale di Fish](https://fishshell.com/docs/current/index.html)

- [Fish Scripting Tutorial](https://fishshell.com/docs/current/tutorial.html)

- [Fish vs Bash vs Zsh](https://unix.stackexchange.com/questions/325289/bash-vs-zsh-vs-fish)