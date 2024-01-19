---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
La conversione di una data in una stringa consiste nel trasformare un formato data (es: 2022-02-17) in un formato leggibile (es: 17 Febbraio 2022). I programmatori lo fanno per rendere le date più comprensibili per gli utenti.

## Come si fa:
Ecco come si può fare utilizzando il comando `date` in Bash:

```Bash
# Impostare la data
data_oggetto=$(date -d "2022-02-17")

# Convertire la data in stringa
data_stringa=$(date -d "${data_oggetto}" +"%d %B %Y")

# Stampare la data convertita
echo "${data_stringa}"
```

Output:

```Bash
17 Febbraio 2022
```

## Approfondimenti
1. **Contesto storico:** Bash è una shell di Unix rilasciata per la prima volta nel 1989. Da allora, è stato costantemente migliorato e aggiornato, inclusa l'aggiunta della funzione `date` che abbiamo usato qui.
2. **Alternative:** Esistono diverse alternative alla conversione delle date in Bash, come l'utilizzo di Perl, Python, o altri linguaggi. L'approccio dipende dalle tue familiarità e competenze con i vari strumenti.
3. **Dettagli implementativi:** Il comando `date` in Bash utilizza la libreria C `strftime` per formattare le date. Ogni parametro che passi a `strftime` (ad es: `%d`, `%B`, `%Y`) viene sostituito con una componente della data.

## Vedi anche
- Manuale Bash: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
- Documentazione `date`: [http://man7.org/linux/man-pages/man1/date.1.html](http://man7.org/linux/man-pages/man1/date.1.html)
- Documentazione `strftime`: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)