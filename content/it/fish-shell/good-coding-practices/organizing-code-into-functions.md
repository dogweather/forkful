---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:30.280210-07:00
description: 'Come fare: In Fish, si scrive una funzione con la parola chiave `function`,
  si assegna un nome e si conclude con `end`. Ecco un esempio semplice.'
lastmod: '2024-03-13T22:44:43.864781-06:00'
model: gpt-4-0125-preview
summary: In Fish, si scrive una funzione con la parola chiave `function`, si assegna
  un nome e si conclude con `end`.
title: Organizzare il codice in funzioni
weight: 18
---

## Come fare:
In Fish, si scrive una funzione con la parola chiave `function`, si assegna un nome e si conclude con `end`. Ecco un esempio semplice:

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

Ora, facciamola salutare un utente:

```fish
function greet
    set user (whoami)
    echo "Ciao, $user!"
end

greet
```

Output:
```
Ciao, il_tuo_username!
```

Per salvarla tra le sessioni, usa `funcsave greet`.

## Approfondimento
Le funzioni di Fish Shell sono come mini-script — puoi inserirvi praticamente qualsiasi cosa. Storicamente, il concetto di funzioni negli script di shell ha risparmiato innumerevoli ore di digitazione ripetitiva e debugging. A differenza dei linguaggi di programmazione come Python, le funzioni di Shell riguardano più la comodità che la struttura.

Alcune shell, come Bash, usano `function` o semplicemente parentesi graffe. Fish si attiene a `function ... end` — chiaro e leggibile. All'interno delle funzioni Fish, si hanno tutti i fronzoli: parametri, variabili locali con `set -l`, e si può anche definire una funzione all'interno di un'altra funzione.

Non avrai bisogno di un valore di `return` perché Fish non è molto focalizzato su quello; l'output della tua funzione è il suo ritorno. E se vuoi funzioni persistenti disponibili per sessioni future, ricorda `funcsave`.

## Vedi Anche
- Il tutorial di fish sulle funzioni: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Comandi per le funzioni
- [function](https://fishshell.com/docs/current/cmds/function.html) — Crea una funzione
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Stampa o cancella funzioni
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Salva la definizione di una funzione nella cartella di caricamento automatico dell'utente
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Modifica interattivamente una funzione
