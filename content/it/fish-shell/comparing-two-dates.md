---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che cosa è e perché?

Comparare due date significa determinare se una data è precedente, successiva o uguale a un'altra. Questa operazione è fondamentale per i programmatori, ad esempio quando devono eseguire un'azione specifica a seconda dell'ordine cronologico delle date.

## Come fare:

La shell di Fish rende facile comparare due date usando il comando `date`. Diamo un'occhiata a un esempio di codice:

```Fish Shell
# Conversione delle date in secondi da un epoch
set data1 (date -d "2022-01-01" +%s)
set data2 (date -d "2022-02-01" +%s)

# Confronto delle date
if test $data1 -gt $data2
    echo "La data1 è successiva alla data2"
else if test $data1 -eq $data2
    echo "La data1 è uguale alla data2"
else
    echo "La data1 è precedente alla data2"
end
```

L'output potrebbe essere:

`La data1 è precedente alla data2`

## Approfondimento:

La comparazione delle date è un concetto molto datato, le cui origini risalgono ai primi sistemi operativi Unix. La shell di Fish, rilasciata nel 2005, ha semplificato molti comandi Unix, tra cui la comparazione delle date.

Ci sono molte alternative per comparare le date, mai come detto, la shell di Fish lo semplifica molto. Perl, Python, JavaScript e altri linguaggi di programmazione offrono metodi potenti per manipolare le date.

Nel nostro esempio di codice, convertiamo le date in secondi passati dal 1° gennaio 1970 (noto come 'epoch time') con il comando `date -d "data" +%s`. Quindi, confrontiamo i secondi utilizzando i comandi di test standard di Fish.

## Vedi anche:

Per informazioni più approfondite sul lavoro con le date in Fish, consulta queste risorse:

- Documentazione ufficiale di Fish: https://fishshell.com/docs/current/ 
- Tutorial su come utilizzare il comando `date`: https://www.tecmint.com/linux-date-command-examples/
- Una panoramica della manipolazione delle date in vari linguaggi di programmazione: https://en.wikipedia.org/wiki/System_time