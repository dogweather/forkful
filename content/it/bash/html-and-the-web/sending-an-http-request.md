---
date: 2024-01-20 17:58:53.359440-07:00
description: "Come Fare: Inviare richieste HTTP non \xE8 una novit\xE0 nel mondo del\
  \ Bash scripting. Storicamente, `wget` ed `curl` sono i comandi pi\xF9 usati. `curl`\
  \ fornisce\u2026"
lastmod: '2024-04-05T21:53:44.359845-06:00'
model: gpt-4-1106-preview
summary: "Inviare richieste HTTP non \xE8 una novit\xE0 nel mondo del Bash scripting."
title: Inviare una richiesta http
weight: 44
---

## Come Fare:
```Bash
# Utilizzo di cURL per effettuare una richiesta GET
curl https://api.example.com/data

# Risposta di esempio
{"nome":"Mario","professione":"Sviluppatore"}

# Utilizzo di cURL con un metodo POST e dati
curl -X POST -H "Content-Type: application/json" -d '{"utente":"Luca"}' https://api.example.com/users

# Utilizzo di wget per scaricare un file
wget https://example.com/file.zip
```

## Approfondimento
Inviare richieste HTTP non è una novità nel mondo del Bash scripting. Storicamente, `wget` ed `curl` sono i comandi più usati. `curl` fornisce un controllo più granulare e supporta un maggior numero di protocolli rispetto a `wget`, che è più mirato allo scaricamento di file. Per eseguire richieste complesse o gestire la concorrenza, si può ricorrere a linguaggi di scripting più potenti come Python o Node.js, ma `curl` rimane uno strumento versatile e sufficiente per la maggior parte delle esigenze. Dal punto di vista dell'implementazione, quando si usa `curl` in uno script bash, è importante gestire correttamente gli errori e l'uscita del comando per garantire l'affidabilità dello script.

## Altre Risorse
- [cURL manuale ufficiale](https://curl.haxx.se/docs/manpage.html)
- [HTTPie – un client HTTP user-friendly per la riga di comando](https://httpie.org/)
- [Bash scripting guide](https://www.gnu.org/software/bash/manual/)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
