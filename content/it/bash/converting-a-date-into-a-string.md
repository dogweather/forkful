---
title:                "Bash: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Convertire una data in una stringa può essere utile quando si desidera visualizzare una data in un formato diverso da quello predefinito o quando si vuole manipolare la data in modo più flessibile. Ad esempio, è possibile convertire una data in una stringa per usarla come titolo di un file o per inserirla in un database.

## Come fare
Per convertire una data in una stringa in Bash, è possibile utilizzare il comando `date` combinato con un formato specifico. Ecco un esempio:

```bash
# Ottieni la data corrente in formato breve
data=$(date +"%m-%d-%Y")

# Stampa la data come stringa
echo "La data di oggi è $data"
```

Il risultato di questo codice sarà:

```
La data di oggi è 08-01-2021
```

Ci sono molti formati diversi che è possibile utilizzare con il comando `date` per convertire una data in una stringa. Ad esempio, il formato `%d/%m/%Y` produrrà una data nel formato giorno/mese/anno. È possibile trovare una lista completa dei formati disponibili nella documentazione ufficiale di Bash.

## Approfondimento
È possibile manipolare ulteriormente la stringa rappresentante la data utilizzando comandi e strumenti tipici di Bash. Ad esempio, è possibile estrarre parti specifiche della data utilizzando il comando `cut`, come mostrato in questo esempio:

```bash
# Ottieni la data corrente in formato esteso
data=$(date +"%A, %B %d, %Y")

# Esegui il comando cut per estrarre solo il giorno della settimana
giorno=$(echo $data | cut -d "," -f 1)

# Stampa il giorno della settimana come stringa
echo "Oggi è $giorno"
```

Il risultato di questo codice sarà:

```
Oggi è Domenica
```

Ci sono molte altre opzioni e comandi che è possibile utilizzare per manipolare una data convertita in una stringa, e la scelta dipenderà dalle esigenze specifiche del progetto.

## Vedi anche
- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Guida Bash per principianti](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)