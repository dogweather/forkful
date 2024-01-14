---
title:    "Bash: Converting una data in una stringa"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è un'operazione comune quando si lavora con dati in Bash. Consente di visualizzare la data in un formato comprensibile per gli utenti e facilita la manipolazione dei dati nel codice.

## Come Fare

Per convertire una data in una stringa, si utilizza il comando `date`. Ad esempio, se si vuole visualizzare la data odierna nel formato giorno/mese/anno, si può utilizzare il seguente codice:

```Bash
echo "$(date +'%d/%m/%Y')"
```

Questo produrrà un output come `14/11/2021`.

Se si desidera personalizzare ulteriormente il formato della data, si possono utilizzare diverse opzioni con il comando `date`. Ad esempio, per aggiungere il giorno della settimana, il nome del mese e l'anno in formato esteso, si può utilizzare il seguente codice:

```Bash
echo "$(date +'%A, %d %B %Y')"
```

Questo produrrà un output come `Domenica, 14 Novembre 2021`.

## Approfondimenti

La sintassi per convertire una data in una stringa utilizzando il comando `date` può risultare leggermente complicata. Per semplificare l'operazione, si possono utilizzare le variabili come supporto. Ad esempio, per convertire la data di nascita in una stringa nel formato dd/mm/yyyy, si può utilizzare il seguente codice:

```Bash
dob="1995/05/20"
echo "${dob:8:2}/${dob:5:2}/${dob:0:4}"
```

Questo produrrà un output come `20/05/1995`.

## Vedi Anche

- [Documentazione di Bash](https://www.gnu.org/software/bash/manual/html_node/)
- [Guida Bash per Principianti](https://linuxconfig.org/a-beginners-guide-to-bash-conditions-and-loops)
- [Uso del comando date in Bash](https://linuxize.com/post/linux-date-command/)