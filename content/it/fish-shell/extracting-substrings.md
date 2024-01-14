---
title:    "Fish Shell: Estrazione di sottostringhe"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Estrazione di sottostringhe è un'attività comune nella programmazione che può essere utile per una varietà di ragioni. Può essere utile per manipolare ed elaborare rapidamente i dati, o semplicemente per ottenere informazioni specifiche da una stringa più grande. In questo articolo, impareremo come e quando utilizzare questa funzionalità nel Fish Shell.

## Come fare

Per estrarre sottostringhe in Fish Shell, utilizziamo il comando `string sub` seguito da due parametri: la posizione iniziale della sottostringa e la sua lunghezza. Possiamo anche utilizzare un terzo parametro per indicare la stringa dalla quale vogliamo estrarre la sottostringa. Ecco un esempio:

```
Fish Shell> string sub 2 5 Hello world!
llo w
```

In questo caso, abbiamo estratto una sottostringa di 5 caratteri a partire dalla posizione 2 della stringa "Hello world!". Nota che i caratteri vengono contati partendo da 0.

Possiamo anche combinare l'uso di `string sub` con altri comandi del Fish Shell per elaborare dati in modo più efficiente. Ad esempio:

```
Fish Shell> echo "Today's date is $(date)" | string sub 10 8
s date i
```

Qui, abbiamo utilizzato `string sub` per estrarre una sottostringa dalla data corrente generata dal comando `date`.

## Approfondimento

Oltre all'uso dei parametri per indicare la posizione e la lunghezza della sottostringa, è possibile anche utilizzare delle "tacche" per riferirsi a una posizione specifica della stringa. Ad esempio, `string sub 1..3` restituirà i primi tre caratteri della stringa.

Possiamo anche combinare diverse sottostringhe utilizzando l'operatore `+`. Ad esempio, `string sub 0 3 + string sub 7 2` restituirà i primi tre caratteri e gli ultimi due caratteri della stringa.

Inoltre, possiamo utilizzare il comando `string length` per ottenere la lunghezza della stringa, che può aiutarci a specificare i parametri per l'estrazione della sottostringa in modo più preciso.

## Vedi anche

- Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/cmds/string.html#sub_9
- Tutorial su estrazione di sottostringhe con Fish Shell: https://devdojo.com/piotrek/introduction-to-fish-shell-part-4-the-string-command