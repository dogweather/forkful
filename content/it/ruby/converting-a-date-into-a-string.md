---
title:    "Ruby: Convertire una data in una stringa"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché

Molti programmatori devono gestire le date all'interno dei loro progetti e a volte è necessario convertire una data in una stringa. In questo articolo, esploreremo il processo di conversione delle date in stringhe utilizzando il linguaggio di programmazione Ruby.

## Come fare

Per convertire una data in una stringa, dobbiamo utilizzare il metodo `strftime` che significa "formattare data e ora" in inglese. Questo metodo accetta un argomento che definisce la formattazione della data e restituisce la data in formato di stringa. Ecco un esempio di codice:

```Ruby
# Dichiarare una variabile contenente una data
date = Time.new(2021, 7, 23)

# Utilizzare il metodo `strftime` per convertire la data in una stringa
format = date.strftime("%d/%m/%Y")

# Stampare il risultato
puts format

# Output: 23/07/2021
```

Come si può vedere nell'esempio, il metodo `strftime` prende come argomento una stringa che contiene dei segnaposto speciali per definire la formattazione della data:

- `%d` rappresenta il giorno del mese con due cifre
- `%m` rappresenta il mese con due cifre
- `%Y` rappresenta l'anno con quattro cifre

Possiamo utilizzare questi segnaposto in qualsiasi ordine e aggiungere ulteriori separatori, come `/`, per formattare la data come desideriamo. Inoltre, possiamo anche aggiungere segnaposto per rappresentare l'ora e i minuti.

```Ruby
format = date.strftime("%m-%d-%Y %H:%M")
# Output: 07-23-2021 00:00
```

## Approfondimento

Oltre ai segnaposto forniti dal metodo `strftime`, Ruby offre una vasta gamma di opzioni per formattare le date in stringhe. Puoi utilizzare questi segnaposto per rappresentare il giorno della settimana, il nome del mese, le 12 ore o le 24 ore, e molto altro ancora.

Inoltre, se desideri conoscere tutte le opzioni di formattazione disponibili, puoi consultare la documentazione di Ruby sul metodo `strftime` o cercare online un elenco dettagliato dei segnaposto.

## Vedi anche

- [Documentazione Ruby sul metodo `strftime`](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Elenco completo di segnaposto per il metodo `strftime`](https://strftime.org/)