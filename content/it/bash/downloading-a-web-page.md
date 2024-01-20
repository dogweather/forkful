---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scaricare una pagina web significa ottenere sul proprio dispositivo i dati che compongono il contenuto visibile su un browser. I programmatori fanno questo per analizzare i dati, accedere a informazioni specifiche o costruire un'archivio.

## Come fare:

Per scaricare una pagina web in Bash, puoi utilizzare il comando `curl` o `wget`. Ecco un esempio con `curl`:

```Bash
curl http://www.example.com -o pagina.html
```

In questo modo, il contenuto di www.example.com verrà salvato in un file chiamato pagina.html. Il risultato apparirà così:

```Bash
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  1270  100  1270    0     0   6335      0 --:--:-- --:--:-- --:--:--  6335
```

## Approfondimento

Anni fa, i programmatori realizzavano download di pagine web utilizzando la linea di comando FTP. Oggi, `curl` e `wget` sono i metodi più comuni. All'interno del contesto di scripting, `wget` è spesso preferito grazie alla sua resilienza e facilità di uso.

`curl` e `wget` non sono gli unici modi per scaricare una pagina web. Altre opzioni includono gli script Perl con LWP e Python con urllib o requests.

Ecco un esempio dettagliato dell'implementazione specifica di `wget`:

```Bash
wget -P /path/to/directory http://www.example.com
```
In questo caso, la pagina web sarà salvata nella cartella specificata (/path/to/directory).

## Vedi anche

Per apprendere di più sull'argomento potete consultare:

- [Curl Man Page](https://curl.se/docs/manpage.html)
- [Wget Man Page](https://www.gnu.org/software/wget/manual/wget.html)
- [Python Requests](https://requests.readthedocs.io/en/master/)

Ricorda che il miglior modo per imparare è sperimentare. Provate differenti approcci e sin dal primo errore imparerete qualcosa di nuovo.