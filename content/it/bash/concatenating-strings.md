---
title:    "Bash: Unione di stringhe."
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione comune quando si lavora con la programmazione Bash. Ci permette di unire due o più stringhe creando una nuova stringa. Ciò può essere utile per creare percorsi di file dinamici, costruire messaggi di output personalizzati e molto altro ancora.

## Come fare

Per concatenare le stringhe in Bash, è necessario utilizzare l'operatore `+` o un trattino `-` tra le stringhe che vogliamo unire. Vediamo un esempio pratico:

```
Bash
# Dichiarazione delle stringhe
stringa1="Ciao"
stringa2="Mondo"

# Concatenazione delle due stringhe
risultato=$stringa1$stringa2

# Stampa del risultato
echo $risultato
```

Nell'esempio sopra, abbiamo dichiarato due stringhe, "Ciao" e "Mondo". Poi, utilizziamo l'operatore di concatenazione `+` per unire le due stringhe e assegnare il risultato alla variabile "risultato". Infine, stampiamo il risultato con l'uso del comando `echo`. 

L'output dovrebbe essere:

```
CiaoMondo
```

Possiamo anche utilizzare il trattino `-` per ottenere lo stesso risultato:

```
risultato=$stringa1-$stringa2
```

Un altro modo per concatenare le stringhe è utilizzando il comando `printf`:

```
risultato=$(printf "%s%s" $stringa1 $stringa2)
```

Questo comando è particolarmente utile quando si vogliono formattare le stringhe prima di concatenarle.

## Approfondimento

Una delle caratteristiche interessanti della concatenazione di stringhe in Bash è che possiamo unire anche numeri interi e float alle stringhe, senza doverli convertire in stringhe prima.

```
Bash
# Dichiarazione di una stringa e un numero
stringa="Il numero è "
numero=10

# Concatenazione
risultato=$stringa$numero

# Stampa del risultato
echo $risultato
```

L'output sarà:

```
Il numero è 10
```

Un'altra cosa da notare è che, quando concateniamo una variabile ad una stringa, non dobbiamo utilizzare spazi tra di loro. Se abbiamo bisogno di uno spazio tra la stringa e la variabile, possiamo utilizzare il carattere di spazio all'interno della stringa stessa.

```
risultato="La variabile è "$variabile # Non sarà inserito uno spazio
risultato="La variabile è "$variabile # Uno spazio verrà inserito
```

## Vedi anche

* [Bash: Concatenare stringhe](https://tecnonucleous.com/2014/07/17/1566/bash-concatenare-stringhe)
* [Guida pratica alla programmazione Bash](https://bit.ly/2pM65HN)
* [Introduzione alla programmazione Bash](https://technedigitale.com/2017/05/25/la-programmazione-bash-unintroduzione)