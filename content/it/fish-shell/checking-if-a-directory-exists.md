---
title:                "Verifica se una directory esiste"
html_title:           "Fish Shell: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Vediamo due cose: (1) cosa significa controllare se una directory esiste: in pratica, significa verificare se una specifica cartella è presente nel tuo sistema di file. (2) E perché i programmatori lo fanno? Per evitare errori. Se provi a manipolare una directory che non esiste, il tuo codice darà un errore.

## Come fare:

Puoi utilizzare il comando 'test' o la sua forma abbreviata '[ ]' per controllare se una directory esiste. Ecco gli esempi di codice:

```Fish Shell
# Utilizzare il comando 'test'
if test -d /path/alla/directory
    echo "La directory esiste."
else
    echo "La directory non esiste."
end
```

O la forma abbreviata '[ ]':

```Fish Shell
# Utilizzare la forma abbreviata '[ ]'
if [ -d /path/alla/directory ]
    echo "La directory esiste."
else
    echo "La directory non esiste."
end
```

Risultato della ricerca:

```
La directory esiste.
```
O
```
La directory non esiste.
```

## Approfondimento

Historicalmente, il comando 'test' è presente nella shell Unix sin dagli anni '70. La forma abbreviata '[ ]' è un alias per 'test'.

Ci sono anche modi alternativi per controllare se una directory esiste in altri linguaggi di programmazione, come Python, Java, JavaScript. C'è anche un approccio più avanzato attraverso l'utilizzo di script di sistema o tool di terze parti come 'find' o 'locate'.

Se stai scrivendo uno script, è spesso utile sapere come funzionano i comandi. Il comando 'test' nel Fish Shell restituisce '0' (vero) se la condizione è soddisfatta, '1' (falso) se non lo è. Quindi, quando usi 'test' o '[ ]' in un'istruzione 'if', stai effettivamente controllando il valore di ritorno del comando 'test'.

## Vedi anche

Per approfondire la programmazione Fish Shell, consulta i seguenti collegamenti:

1. Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/index.html
2. Tutorial su come scrivere script con Fish Shell: https://fishshell.com/docs/current/tutorial.html
3. Discussione dettagliata sul comando 'test' in Unix: https://unix.stackexchange.com/questions/32134/what-is-the-purpose-of-the-test-command
4. Come controllare se una directory esiste in Python, Java, JavaScript: https://stackoverflow.com/questions/tagged/directory-exists.