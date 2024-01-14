---
title:                "Fish Shell: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date può essere molto utile quando si lavora con dati temporali, sia per scopi di analisi che di gestione di progetti. Con il Fish Shell è possibile utilizzare alcuni semplici comandi per realizzare questa operazione in modo efficiente ed accurato.

## Come fare

Per iniziare, assicurati di avere installato il Fish Shell sul tuo sistema. Una volta fatto ciò, puoi usare il comando `date` per visualizzare la data e l'ora attuali. Ad esempio:

```Fish Shell
echo (date)
```

Questo restituirà un output simile a questo:

```
gio lug  8 09:45:12 CEST 2021
```

Per confrontare due date, puoi utilizzare il comando `strftime` insieme al formato desiderato delle date. Ecco un esempio:

```Fish Shell
set data1 (date +%Y%m%d) #imposta la prima data
set data2 (date +%Y%m%d -d "tomorrow") #imposta la seconda data come domani
```

Puoi quindi utilizzare il comando `test` per confrontare le due date. Ad esempio:

```Fish Shell
if test $data1 -gt $data2
    echo "La prima data è successiva alla seconda data"
else if test $data1 -lt $data2
    echo "La prima data è precedente alla seconda data"
else
    echo "Le due date sono uguali"
end
```

Questo restituirà un output simile a questo:

```
La prima data è precedente alla seconda data
```

## Approfondimento

Il formato utilizzato con il comando `date` è basato sulle specifiche di POSIX e può essere personalizzato in base alle proprie esigenze. Ad esempio, `%Y` rappresenta l'anno a quattro cifre, `%m` rappresenta il mese con due cifre e `%d` rappresenta il giorno con due cifre. Per maggiori informazioni sulle specifiche dei comandi, puoi consultare la documentazione ufficiale del Fish Shell.

## Vedi anche

- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sulle date in Fish Shell](https://www.howtogeek.com/522837/how-to-use-the-linux-date-command-to-track-time/)
- [Formato delle date in Fish Shell](https://fishshell.com/docs/current/index.html#strftime-time)