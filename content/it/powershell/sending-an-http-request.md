---
title:                "Inviare una richiesta http"
html_title:           "PowerShell: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
In poche parole, inviare una richiesta HTTP significa comunicare con un server attraverso il protocollo HTTP (Hypertext Transfer Protocol). Questo può essere fatto con diversi obiettivi tra cui: scaricare dati da un server, aggiornare informazioni sul server o semplicemente scambiare informazioni tra client e server.

## Come fare:
Utilizzare il comando `Invoke-WebRequest` in PowerShell per inviare una richiesta HTTP. Ecco un esempio di codice:

```PowerShell
$request = Invoke-WebRequest -Uri "https://www.example.com"
```

In questo esempio, il comando `Invoke-WebRequest` viene utilizzato per inviare una richiesta GET al server di "www.example.com". Il risultato della richiesta viene memorizzato nella variabile `$request`.

Ecco un esempio più dettagliato di come inviare una richiesta POST con body e header:

```PowerShell
# Configurazione dell'header
$header = @{
    "Content-Type" = "application/json"
}

$data = @{
    "username" = "esempio"
    "password" = "passwordsegreta"
}

# Invio della richiesta POST
$response = Invoke-WebRequest -Uri "https://www.example.com/login" -Method POST -Headers $header -Body (ConvertTo-Json $data)
```

In questo esempio, viene creato un header con il tipo "Content-Type" impostato su "application/json" per specificare al server il formato dei dati che verranno inviati nel body della richiesta. Poi viene creato un oggetto `$data` che viene convertito in formato JSON utilizzando il cmdlet `ConvertTo-Json`. Infine, viene inviata la richiesta POST al server di "www.example.com/login" con l'header e il body specificati nella variabile `$response` che contiene il risultato della richiesta.

## Deep Dive:
Nella storia dell'informatica, l'invio delle richieste HTTP è stato uno degli sviluppi più importanti per rendere il web così potente e accessibile. Oggi ci sono diverse alternative per inviare richieste HTTP come cURL o wget, tuttavia, PowerShell offre un modo semplice e integrato per eseguire questo compito.

Ci sono alcune opzioni aggiuntive da tenere in considerazione quando si utilizza il comando `Invoke-WebRequest` come ad esempio l'utilizzo dei parametri `-UseBasicParsing` e `-UseDefaultCredentials`. Inoltre, si possono aggiungere altre opzioni di configurazione per gli header e il body della richiesta.

## Vedi anche:
Per ulteriori informazioni su come utilizzare il comando `Invoke-WebRequest` in PowerShell, si consiglia di consultare la documentazione ufficiale di Microsoft su [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7). Inoltre, puoi trovare ulteriori esempi e informazioni su [TechNet Gallery](https://gallery.technet.microsoft.com/scriptcenter/Sending-HTTP-requests-4d181955) e [GitHub](https://github.com/MicrosoftDocs/PowerShell-Docs/tree/live/reference/5.0/Microsoft.PowerShell.Utility/Invoke-WebRequest).