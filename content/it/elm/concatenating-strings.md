---
title:    "Elm: Concatenazione di stringhe"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un'operazione comune nella programmazione, utilizzata per unire due o più stringhe in una sola. Questa operazione può essere utile in molte situazioni, come ad esempio la creazione di un messaggio di saluto personalizzato o la formattazione di una data.

## Come Fare

Per concatenare stringhe in Elm, è possibile utilizzare l'operatore `++` o la funzione built-in `String.concat`. Ad esempio:

```Elm
saluto = "Ciao "
nome = "Gianni"

salutoPersonalizzato = saluto ++ nome  -- "Ciao Gianni"

date = ["01", "02", "2021"]

dataFormattata = String.concat "-" date  -- "01-02-2021"
```

## Approfondimento

Il simbolo `++` nelle espressioni di concatenazione non è semplicemente una shorthand per la funzione `String.concat`. In realtà, è un operatore infisso, il che significa che prende due argomenti tra cui essere posto. Inoltre, se si tenta di concatenare tipi diversi, verrà restituito un errore di compilazione.

È importante notare che, poiché le stringhe in Elm sono immutabili, ogni operazione di concatenazione creerà una nuova stringa invece di modificare l'originale.

## Vedi Anche

- [Documentazione di Elm su stringhe](https://elm-lang.org/docs/strings)
- [Un articolo su come utilizzare al meglio la concatenazione di stringhe in Elm](https://blog.jenkster.com/2017/03/concat-string.html)
- [Un tutorial su Elm](https://tutorialspoint.com/elm/index.htm)