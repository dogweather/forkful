---
title:                "Bash: Calcolare una data nel futuro o nel passato"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data in futuro o passato può essere utile in numerose situazioni, ad esempio per pianificare eventi, gestire scadenze o effettuare previsioni.

## Come fare

Per calcolare una data in futuro o passato in Bash, possiamo utilizzare il comando `date` e le relative opzioni. Ad esempio, per ottenere la data di domani possiamo scrivere:

```Bash
date -d "tomorrow"
```

Questo restituirà la data di domani nel formato standard `giorno mese anno`. Possiamo anche specificare un offset di giorni, ad esempio per ottenere la data di 5 giorni fa:

```Bash
date -d "5 days ago"
```

Inoltre, possiamo specificare una data di partenza e un formato di output personalizzato. Ad esempio, per ottenere la data odierna in formato `anno-mese-giorno`, possiamo scrivere:

```Bash
date -d "today" +"%Y-%m-%d"
```

## Approfondimento

Il comando `date` offre molte opzioni per calcolare date in futuro o passato, come ad esempio specificare un orario preciso o un intervallo temporale più ampio. È inoltre possibile utilizzare il comando in combinazione con altri comandi Bash per ottenere risultati più complessi.

Un altro strumento utile per calcolare date in Bash è il pacchetto `dateutils`, che offre una vasta gamma di funzionalità per gestire date e tempi in modo preciso e flessibile.

## Vedi anche

- Documentazione ufficiale di `date`: https://bash.cyberciti.biz/guide/The_date_command
- Guida di riferimento di `dateutils`: https://www.commandlinefu.com/commands/view/11527/calculate-days-on-any-date-using-dateutils
- Tutorial su come utilizzare `date` e `dateutils`: https://www.linuxjournal.com/content/working-date-and-time-bash