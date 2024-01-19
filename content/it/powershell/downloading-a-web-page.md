---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Scaricare Una Pagina Web Usando PowerShell

## Cos'è E Perché?
Scaricare una pagina web significa prelevare il codice HTML di una pagina di internet per usarlo altrove. I programmatori spesso lo fanno per analizzare o manipolare i dati in essa contenuti.

## Come si fa:
Usando PowerShell, possiamo scaricare una pagina web con poche righe di codice. Prendiamo come esempio il sito "example.com".

```PowerShell
# Inizializza la richiesta web
$url = "http://example.com"
$webclient = New-Object System.Net.WebClient

# Scarica il contenuto HTML
$html = $webclient.DownloadString($url)

# Visualizza il codice
$html
```

L'output sarà il codice HTML di "example.com".

## Approfondimenti:
Historicalmente, PowerShell nasce come shell di comando e scripting per piattaforme Windows e la sua prima versione è stata rilasciata nel 2006. Ha da allora evoluto per includere le funzionalità HTTP come l'esempio presentato.

Esistono alternative a PowerShell per scaricare pagine web, come curl o wget nel mondo Linux. Anche altri linguaggi di programmazione come Python o JavaScript hanno librerie dedicate per la gestione delle richieste HTTP.

Per quanto riguarda i dettagli di implementazione, `New-Object System.Net.WebClient` crea un nuovo oggetto WebClient che fornisce metodi comuni per inviare dati a e ricevere da un qualsiasi URL di risorsa. Il metodo `DownloadString` quindi scarica il contenuto del sito come stringa.

## Vedi Anche:
Per più informazioni su PowerShell:
- Documentazione Ufficiale PowerShell: https://docs.microsoft.com/it-it/powershell/
- Gestire le richieste HTTP in PowerShell: https://4sysops.com/archives/using-powershell-as-a-rest-client/
- Come scaricare file con PowerShell: https://blog.jourdant.me/post/3-ways-to-download-files-with-powershell