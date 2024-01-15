---
title:                "Conversione di una data in una stringa"
html_title:           "Fish Shell: Conversione di una data in una stringa"
simple_title:         "Conversione di una data in una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è un'operazione importante per lavorare con le date in modo più flessibile e leggibile. Ad esempio, potresti voler convertire una data in formato numerico in una più comprensibile serie di caratteri, come "13 aprile 2021".

## Come Fare

Per convertire una data in una stringa utilizzando Fish Shell, è necessario utilizzare il comando `date` combinato con il comando `string` e i loro rispettivi argomenti.

```Fish Shell
set data (date "+%d %B %Y") 
echo (string replace $data "-" " ") 
# Output: 13 aprile 2021
```

Nell'esempio sopra abbiamo utilizzato il comando `date` con l'argomento `"+%d %B %Y"` per ottenere la data corrente in formato numerico con il giorno, il mese e l'anno. Successivamente, con il comando `string replace` abbiamo sostituito il simbolo "-" con uno spazio vuoto, ottenendo così la data desiderata in formato di stringa.

## Deep Dive

Oltre alla semplice conversione di una data in una stringa, Fish Shell offre anche la possibilità di personalizzare il formato della data utilizzando i seguenti argomenti con il comando `date`:

- `%a` per il nome dell'abbreviazione del giorno della settimana (es. Lun, Mar, Mer)
- `%A` per il nome completo del giorno della settimana (es. Lunedi, Martedì, Mercoledì)
- `%b` per il nome dell'abbreviazione del mese (es. Gen, Feb, Mar)
- `%B` per il nome completo del mese (es. Gennaio, Febbraio, Marzo)
- `%c` per la data e l'ora complete (es. Mon Jan 25 18:15:22 EST 2021)
- `%d` per il giorno del mese (es. 25)
- `%H` per l'ora in formato 24 ore (es. 18)
- `%I` per l'ora in formato 12 ore (es. 06)
- `%m` per il numero del mese (es. 01, 02, 03)
- `%M` per i minuti (es. 15)
- `%p` per indicare AM o PM (es. AM, PM)
- `%S` per i secondi (es. 22)
- `%Y` per l'anno completo (es. 2021)

Inoltre, è possibile combinare questi argomenti per ottenere un formato personalizzato della data. Ad esempio, utilizzando `+%d %B %Y` otterremo la data come "25 gennaio 2021".

## See Also

Per ulteriori informazioni sui comandi `date` e `string` e sui loro argomenti, puoi consultare la documentazione ufficiale di Fish Shell:

- [Comando `date`](https://fishshell.com/docs/current/cmds/date.html)
- [Comando `string`](https://fishshell.com/docs/current/cmds/string.html)