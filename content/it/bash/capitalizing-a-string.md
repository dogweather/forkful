---
title:                "Maiuscolo di una stringa"
html_title:           "Bash: Maiuscolo di una stringa"
simple_title:         "Maiuscolo di una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è una pratica comune quando si vuole dare più enfasi o evidenziare una parola o una frase in un testo. Può essere utile anche per uniformare il formato di una stringa in un sistema o codice.

## Come fare

Per capitalizzare una stringa in Bash, si possono utilizzare vari comandi e metodi. Ecco alcuni esempi di codice con il relativo output:

```
# Utilizzando il comando "tr" per trasformare tutti i caratteri in maiuscolo
echo "ciao mondo!" | tr '[:lower:]' '[:upper:]'
CIAO MONDO!

# Utilizzando la variabile di stringa "${name^^}" – disponibile in Bash 4+
name="pippo"
echo "${name^^}"
PIPPO

# Utilizzando il comando "sed" per sostituire la prima lettera di ogni parola con il carattere maiuscolo
echo "questa è una frase di prova" | sed -e "s/\b\(.\)/\u\1/g"
Questa È Una Frase Di Prova
```

È possibile anche creare una funzione personalizzata che sfrutti il ciclo "for" per ciclare attraverso ogni carattere della stringa e utilizzare il comando "printf" per stampare la versione maiuscola di ogni carattere.

## Approfondimento

È importante notare che la capitalizzazione di una stringa in Bash dipende anche dalla lingua e dalle impostazioni del sistema in uso. Ad esempio, in alcune lingue la lettera "i" maiuscola può presentarsi come "İ" anziché "I". Perciò, è sempre consigliato testare e verificare il risultato finale a seconda del contesto in cui si sta utilizzando la stringa capitalizzata.

## Vedi anche

- [Bash: Capitale Funzione su nixCraft](https://www.cyberciti.biz/tips/bashing-uppercase-lowercase-conversion.html)
- [Stringhe in Bash: Capitale su The Bash Guide](https://mywiki.wooledge.org/BashFAQ/071)
- [Capitale Stringa in Bash su LinuxConfig.org](https://linuxconfig.org/bash-string-capitalization-tutorial)