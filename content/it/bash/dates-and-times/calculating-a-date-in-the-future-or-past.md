---
title:                "Calcolo di una data futura o passata"
date:                  2024-01-20T17:28:42.737851-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calcolare le date nel futuro o nel passato significa determinare un giorno specifico prima o dopo una data nota. I programmatori lo fanno per gestire scadenze, pianificazioni o per tracciare eventi nel tempo.

## How to:
Calcoliamo le date usando il comando `date` in Bash. Ecco degli esempi:

```Bash
# Data attuale
oggi=$(date '+%Y-%m-%d')
echo "Oggi è: $oggi"

# Aggiungi 10 giorni
data_futura=$(date -d "$oggi + 10 days" '+%Y-%m-%d')
echo "Tra 10 giorni sarà: $data_futura"

# Rimuovi 20 giorni
data_passata=$(date -d "$oggi - 20 days" '+%Y-%m-%d')
echo "20 giorni fa è stato: $data_passata"
```
Output previsto:
```
Oggi è: 2023-04-01
Tra 10 giorni sarà: 2023-04-11
20 giorni fa è stato: 2023-03-12
```

## Deep Dive
Il comando `date` in Unix è vecchio quanto i computer stessi. Era già presente in versioni iniziali di Unix. `date` permette di visualizzare o impostare la data e l'ora del sistema.

Alternative:
- `gdate`: su sistemi non GNU (come macOS), potresti usare `gdate` se installi `coreutils`.
- Altre lingue di scripting (Python, Perl): potrebbero offrire più flessibilità e funzioni per la gestione delle date.

Dettagli implementativi:
- Bash non ha una gestione integrata delle date. Si appoggia al comando `date` del sistema.
- L'opzione `-d` di `date` permette di passare una stringa che descrive la data desiderata.
- Attenzione alle configurazioni regionali: la data può essere interpretata in modi diversi. Per esempio, in alcune localizzazioni, il formato di default potrebbe essere `gg/mm/aaaa` invece di `aaaa-mm-gg`.

## See Also
- Man page di `date` per altre opzioni: [man7.org](http://man7.org/linux/man-pages/man1/date.1.html)
- Guida alla gestione delle date in bash: [Bash Date Guide](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- Documentazione GNU coreutils `date`: [GNU coreutils date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
