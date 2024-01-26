---
title:                "Organizzazione del codice in funzioni"
date:                  2024-01-26T01:10:07.962551-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizzazione del codice in funzioni"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Organizzare il codice in funzioni significa raggruppare parti di script per svolgere compiti specifici. Lo facciamo perché rende il codice più facile da leggere, da testare e da riutilizzare — nessuno vuole dover navigare in un pantano di spaghetti di codice.

## Come fare:
In Fish, si scrive una funzione con la parola chiave `function`, la si nomina e si conclude con `end`. Eccone una semplice:

```fish
function hello
    echo "Ciao, Mondo!"
end

hello
```

Output:
```
Ciao, Mondo!
```

Ora, facciamola diventare un saluto personalizzato per l'utente:

```fish
function greet
    set user (whoami)
    echo "Ciao là, $user!"
end

greet
```

Output:
```
Ciao là, tuo_username!
```

Per salvarla e mantenerla tra le sessioni, usa `funcsave greet`.

## Approfondimento
Le funzioni di Fish Shell sono come mini-script — si può inserirvi praticamente qualsiasi cosa. Storicamente, il concetto di funzioni negli script di shell ha risparmiato innumerevoli ore di digitazione e debug ripetitivi. A differenza di linguaggi di programmazione come Python, le funzioni Shell sono più una questione di comodità che di struttura.

Alcune shell, come Bash, usano `function` o semplicemente le parentesi graffe. Fish si attiene a `function ... end` — chiaro e leggibile. All'interno delle funzioni di Fish, si hanno tutti i fronzoli: parametri, variabili locali con `set -l`, e si può persino definire una funzione all'interno di un'altra.

Non avrai bisogno di un valore di `return` perché Fish non lo enfatizza particolarmente; l'output della tua funzione è il suo valore di ritorno. E se vuoi funzioni persistenti disponibili per le sessioni future, ricorda `funcsave`.

## Vedi Anche
- Il tutorial di fish sulle funzioni: https://fishshell.com/docs/current/tutorial.html#tut_functions
- La documentazione di fish per `function`: https://fishshell.com/docs/current/cmds/function.html
- Una guida estensiva sulla scrittura di funzioni in fish: https://fishshell.com/docs/current/index.html#syntax-function