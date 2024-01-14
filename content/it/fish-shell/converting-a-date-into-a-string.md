---
title:    "Fish Shell: Convertire una data in una stringa"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Perché: Converting una data in una stringa è un'operazione comune nella programmazione per poter visualizzare una data in un formato personalizzato.

Come: Un esempio di come convertire una data in una stringa utilizzando Fish Shell:

```Fish Shell
set date (date -d "today" +"%d-%m-%Y")
echo $date
```

Questo codice imposterà la variabile `date` con la data odierna nel formato "giorno-mese-anno" e la visualizzerà come output.

Per un output più specifico, si possono utilizzare anche altri parametri come l'ora, i minuti e i secondi della data.

Deep Dive: La conversione di una data in una stringa può essere fatta utilizzando diverse funzioni, come `date` e `strftime`. Entrambe accettano un argomento di input, che nel nostro esempio è "today", e un formato per la stringa di output.

Per esempio, il comando `date -d "today" +"%m/%d/%Y"` restituirà la data odierna nel formato "mese/giorno/anno". Oltre alle informazioni sulla data, è anche possibile aggiungere informazioni sul fuso orario o sull'ora locale.

See Also (Vedi anche):
- [Manuale di Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Documentazione di strftime](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/strftim.htm)
- [Tutorial di Fish Shell](https://fishshell.com/docs/current/tutorial.html)