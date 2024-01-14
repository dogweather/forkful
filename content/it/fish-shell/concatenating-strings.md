---
title:    "Fish Shell: Unendo stringhe"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

La concatenazione delle stringhe è un'operazione comune nei linguaggi di programmazione e può essere molto utile quando si lavora con i dati. Attraverso questa tecnica, è possibile unire più stringhe di testo in una sola e manipolarle in varie modi. Vediamo come utilizzarlo nel Fish Shell!

## Come Fare

Per concatenare le stringhe nel Fish Shell, utilizzeremo l'operatore di concatenazione, che è rappresentato dal simbolo `+`. Vediamo un esempio di codice:

```Fish Shell
set nome "Mario"
set cognome "Rossi"
echo $nome $cognome
```

L'output di questo codice sarà `Mario Rossi`, poiché le due stringhe sono state unite grazie all'operatore `+`. Possiamo anche aggiungere del testo tra le due stringhe, ad esempio:

```Fish Shell
echo "Il mio nome è: " $nome $cognome
```

L'output sarà `Il mio nome è: Mario Rossi`. In questo modo possiamo creare stringhe più complesse, aggiungendo anche variabili o testo aggiuntivo.

## Approfondimento

La concatenazione delle stringhe può anche essere utilizzata per creare loop o costruire URL dinamici. È importante prestare attenzione agli spazi tra le stringhe, in quanto possono influire sul risultato finale. Inoltre, è possibile utilizzare l'operatore `+=` per concatenare una stringa al contenuto esistente di una variabile. Ad esempio:

```Fish Shell
set testo "Questo è un"
set testo $testo "esempio"
echo $testo
```

L'output sarà `Questo è un esempio`, poiché abbiamo aggiunto la parola "esempio" al contenuto della variabile `testo`.

## Vedi Anche

- [Guida per iniziare con il Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial avanzato per il Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-advanced)