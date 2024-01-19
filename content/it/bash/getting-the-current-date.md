---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente in bash si riferisce all'acquisizione del giorno attuale nel formato desiderato. È utile per registrare quando accade un evento, come il logging di un'azione o la creazione di un backup.

## Come fare:

Ottenere la data attuale è facile con il comando `date`.

```Bash
# Stampa data e ora correnti nel formato predefinito
date
```
Produzione di esempio:

```
Wed Dec 15 11:45:29 PST 2021
```
 
Possiamo anche formattare l'output utilizzando l'opzione `date +format`. Ecco un esempio:

```Bash
# Stampa la data nel formato YYYY-MM-DD
date +"%Y-%m-%d"
```
Produzione di esempio:

```
2021-12-15
```

## Approfondimento

Il comando `date` è stato introdotto per la prima volta nel Unix Version 7 nel 1979. È rimasto fondamentalmente lo stesso da allora, con alcune aggiunte per supportare opzioni di formattazione più complesse.

Ci sono alternative al comando `date`. Ad esempio, potresti usare Perl o Python, che offrono librerie di gestione del tempo più flessibili. Ma in Bash, `date` è il modo più semplice e diretto.

Una cosa da notare è che il comando `date` ottiene l'ora del sistema, quindi il suo output dipende dalle impostazioni del sistema, inclusi la zona oraria e il formato della data.

## Vedere Anche

Documentazione ufficiale GNU Bash: https://www.gnu.org/software/bash/manual/bash.html

Guida su come usare il comando `date`: https://www.cyberciti.biz/faq/unix-linux-date-format-examples/

Maggiori dettagli sulla formattazione della data: https://www.lifewire.com/format-date-command-linux-4059723