---
date: 2024-01-20 17:43:19.436091-07:00
description: "Scaricare una pagina web significa prelevare tutti i dati che la compongono\
  \ dalla rete per salvarla localmente. I programmatori fanno ci\xF2 per analizzare\u2026"
lastmod: 2024-02-19 22:05:02.669658
model: gpt-4-1106-preview
summary: "Scaricare una pagina web significa prelevare tutti i dati che la compongono\
  \ dalla rete per salvarla localmente. I programmatori fanno ci\xF2 per analizzare\u2026"
title: Scaricare una pagina web
---

{{< edit_this_page >}}

## What & Why?
Scaricare una pagina web significa prelevare tutti i dati che la compongono dalla rete per salvarla localmente. I programmatori fanno ciò per analizzare contenuti, testare la presenza online o automatizzare interazioni con siti web.

## How to:
Per scaricare una pagina web con Bash, si può usare `curl` o `wget`. Ecco alcuni esempi:

```Bash
# Usando curl
curl http://esempio.com -o pagina.html

# Visualizzare il contenuto in console
curl http://esempio.com

# Usando wget
wget http://esempio.com

# Salvare la pagina con un nome specifico
wget -O nomepagina.html http://esempio.com
```

Esempio di output per `curl`:

```Bash
<!DOCTYPE html>
<html>
<head>
    <title>Esempio Pagina</title>
</head>
<body>
    ...
</body>
</html>
```

## Deep Dive:
`curl` e `wget` sono i due cavalli di battaglia per il download di contenuti web da linea di comando. `curl` risale al 1997 e `wget` al 1996, robusti e ricchi di funzionalità. Mentre `curl` supporta una vasta gamma di protocolli, `wget` è spesso la scelta per scaricamenti ricorsivi e mirati. Funzionano su qualsiasi distro Linux e sono spesso preinstallati.

- Dettagli `curl`:
  - Supporta DICT, FILE, FTP, FTPS, Gopher, HTTP, HTTPS, IMAP, IMAPS, LDAP, LDAPS, MQTT, POP3, POP3S, RTMP, RTSP, SCP, SFTP, SMB, SMBS, SMTP, SMTPS, Telnet e TFTP.
  - Opzioni per tracciare header HTTP, upload file, configurare timeout.

- Dettagli `wget`:
  - Recupera contenuti da server HTTP, HTTPS, e FTP.
  - Opzioni per navigazione offline, scaricamenti ricorsivi.

Con `wget` e `curl` si possono anche simulare richieste POST, gestire cookie, e automatizzare attraverso script.

## See Also:
- Documentazione `curl`: https://curl.se/docs/
- Documentazione `wget`: https://www.gnu.org/software/wget/manual/wget.html
- Bash Scripting Tutorial: https://www.shellscript.sh/

Quando hai esigenze di download più complesse, come ad esempio parsing o interazione dinamica, considera strumenti come `Beautiful Soup` per Python o `puppeteer` per Node.js.
