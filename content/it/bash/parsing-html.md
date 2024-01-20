---
title:                "Analisi sintattica dell'html"
html_title:           "Bash: Analisi sintattica dell'html"
simple_title:         "Analisi sintattica dell'html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/parsing-html.md"
---

{{< edit_this_page >}}

# Bash per Parsing HTML

## Cos'è & Perché?
Il parsing HTML invita il tuo codice a leggere ed interpretare il codice HTML. I programmatori lo fanno per estrarre dati, manipolare contenuti e interagire con le pagine web in modi nuovi e creativi.

## Come fare:
Ecco un esempio semplice utilizzando `wget` e `grep`.

```Bash
# Scarica la pagina
wget https://www.esempio.com -O esempio.html

# Ottieni il titolo
grep -oP '(?<=<title>).*(?=</title>)' esempio.html
```

Ecco un altro esempio utilizzando `curl` e `awk`.

```Bash
# Scarica la pagina
curl https://www.esempio.com -o esempio.html

# Ottieni il titolo
awk -v RS='</title>' '/<title>/{gsub(/.*<title>|\n+/,"");print;exit}' esempio.html
```

Questi comandi semplificati restituiscono il titolo del documento HTML.

## Approfondimenti
### Contesto Storico
Inizialmente, il parsing HTML era una operazione complessa che richiedeva librerie software dedicate al calcolo. Oggi, Bash lo rende più semplice con strumenti come `wget`, `curl`, `grep` e `awk`.

### Alternative
Esistono alternative a Bash per il parsing HTML, tra cui Python (con BeautifulSoup o PyQuery), PHP (con DOM), e JavaScript (con jQuery o JSDOM).

### Dettagli di implementazione
Il parsing HTML è un'attività di basso livello e gli strumenti di parsing devono essere in grado di gestire non solo il corretto codice HTML, ma anche il mal formato.

## Vedi Anche
1. [Guida di Bash](https://www.gnu.org/software/bash/manual/bash.html)
2. [Manuale di Grep](https://www.gnu.org/software/grep/manual/grep.html)
3. [Manuale di AWK](https://www.gnu.org/software/gawk/manual/gawk.html)
4. [Documentazione di Wget](https://www.gnu.org/software/wget/manual/wget.html)
5. [Documentazione di Curl](https://curl.haxx.se/docs/manpage.html)