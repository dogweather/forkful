---
title:                "Fish Shell: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler estrarre sottostringhe all'interno del tuo codice Fish Shell. Ad esempio, potresti dover manipolare stringhe per creare nuove variabili o per formattare i risultati in un modo specifico.

## Come Fare

Estrarre sottostringhe in Fish Shell è relativamente semplice. Per farlo, usa il comando `string sub` seguito da un indice di inizio e fine, seguito dalla stringa a cui si vuole applicare l'estrazione.

```
Fish Shell
string sub 2 6 "Ciao a tutti!" 
```

Questo codice restituirà la sottostringa "iao a" poiché l'indice di inizio è 2 e l'indice finale è 6, che corrisponde alle lettere "i" e "a" rispettivamente.

Puoi anche combinare l'uso di `string sub` con altri comandi per creare espressioni più complesse. Ad esempio, puoi usare il comando `string match` per trovare una determinata parola all'interno di una stringa e poi estrarne la sottostringa usando `string sub`.

## Approfondimento

L'estrazione delle sottostringhe è solo una delle tante funzioni utili che puoi sfruttare in Fish Shell. Per saperne di più, puoi consultare la documentazione ufficiale di Fish Shell o partecipare alla comunità di utenti per scambiare informazioni e scoprire nuovi trucchi e tecniche.

## Vedi Anche

- [Documentazione di Fish Shell](https://fishshell.com/docs/current/index.html#string-substring)
- [Comunità di Fish Shell](https://fishshell.com/community.html)