---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

La conversione di una stringa in minuscolo è un'operazione comune in programmazione, in cui tutte le lettere maiuscole in una stringa vengono convertite in minuscole. Spesso gli sviluppatori lo fanno per facilitare confronti stringa insensibili al caso o per manipolare o standardizzare i dati.

## Come fare:

Ecco un esempio su come convertire una stringa in minuscolo in Bash:

```Bash
stringa="Ho La Febbre DA CAVALLO"
echo "${stringa,,}"
```

Questo restituirà:

```
ho la febbre da cavallo
```

## Approfondimento:

La conversione di stringhe in Bash non ha una lunga storia, dato che è stato aggiunto solo nelle versioni recenti di Bash (4.0 e successive). Prima di questo, gli sviluppatori avrebbero dovuto utilizzare comandi esterni come `tr` per ottenere lo stesso risultato.

Riguardo alle alternative, potresti utilizzare `awk` o `tr` per convertire una stringa in minuscolo. Ecco come si può fare con `tr`:

```Bash
echo "Ho La Febbre DA CAVALLO" | tr '[:upper:]' '[:lower:]'
```

Questo produrrà lo stesso output di prima.

Infine, la conversione di stringhe in Bash sfrutta la funzione `towlower` della libreria standard del C. Questa funzione prende un carattere wid e restituisce la sua versione in minuscolo. Bash esegue questa operazione per ogni carattere nella stringa.

## Vedi Anche:

Per saperne di più sulla manipolazione delle stringhe in Bash, dai un'occhiata a questi collegamenti:

- [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash String Manipulation Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)