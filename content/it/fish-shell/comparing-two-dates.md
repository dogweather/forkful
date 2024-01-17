---
title:                "Confronto tra due date"
html_title:           "Fish Shell: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Confrontare due date è un'operazione comune nella programmazione. Ciò ci consente di determinare la differenza tra due istanti nel tempo e utilizzarla per eseguire calcoli o prendere decisioni nelle nostre applicazioni. Ad esempio, possiamo confrontare la data di nascita di un utente con la data odierna per verificare se ha raggiunto la maggiore età.

## Come fare:

Un modo semplice per confrontare due date in Fish Shell è utilizzare il comando `date` per convertire le date in timestamp e quindi utilizzare l'operatore di sottrazione per ottenere la differenza in secondi. Ad esempio:

```Fish Shell
set date1 (date -r "2021-01-01" "+%s")
set date2 (date -r "2021-01-05" "+%s")

echo "La differenza è di: " (math $date2 - $date1) "secondi"
```

Questo codice ci restituirà la differenza in secondi tra il 1 gennaio 2021 e il 5 gennaio 2021, ovvero 345600 secondi. Possiamo anche formattare la data in un modo più leggibile utilizzando il comando `date` con l'opzione `-Iseconds`, come mostrato di seguito:

```Fish Shell
echo "La differenza è di: " (date -Ir $date2) "secondi"
```

Questo ci mostrerà la differenza in un formato più comodo, ad esempio "4 days, 0 hours, 0 minutes, 0 seconds".

## Approfondimento:

L'utilizzo del timestamp per confrontare due date è diventato popolare già negli anni `70, grazie all'introduzione di Unix. Inoltre, esistono anche altri approcci per confrontare date, come utilizzare librerie specifiche di programmazione o utilizzare un formato standard per le date come ISO 8601. È importante considerare il tipo di applicazione che stiamo sviluppando e scegliere il metodo più appropriato per il nostro caso.

## Vedi anche:

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/commands.html)
- [ISO 8601 standard](https://en.wikipedia.org/wiki/ISO_8601)
- [Confronto di date in altri linguaggi di programmazione](https://www.techiedelight.com/compare-dates-without-using-library-java-cpp-python/)