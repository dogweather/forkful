---
title:                "Logboekregistratie"
date:                  2024-01-28T22:03:02.469333-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Loggen draait allemaal om het bijhouden van een registratie van gebeurtenissen, toestanden en datastromen binnen een app. Programmeurs doen dit om bugs te diagnosticeren, de prestaties te monitoren en de operationele gezondheid van de app te volgen—het maakt het in feite de software-equivalent van een zwarte doos in vliegtuigen.

## Hoe:
In Go kan loggen op meerdere manieren worden afgehandeld, variërend van het standaardbibliotheek 'log'-pakket tot third-party bibliotheken zoals `logrus` en `zap`. Hier is een eenvoudig voorbeeld met het ingebouwde `log`-pakket:

```Go
package main

import (
    "log"
    "os"
)

func main() {
    // Maak een logbestand
    logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
    if err != nil {
        log.Fatal(err)
    }
    defer logFile.Close()

    // Stel de loguitvoer in op het bestand
    log.SetOutput(logFile)

    // Log wat gebeurtenissen
    log.Println("Starten van de applicatie...")
    // ... applicatielogica hier ...
    log.Println("Applicatie succesvol beëindigd.")
}
```

Als je deze code draait, zie je geen uitvoer naar de terminal omdat alles naar `app.log` gaat. Hier is een kijkje in wat je in dat logbestand zou vinden:

```
2023/01/02 15:04:05 Starten van de applicatie...
2023/01/02 15:05:01 Applicatie succesvol beëindigd.
```

## Diepgaand
Loggen in programmeren gaat terug tot de eerste computers, waar ingenieurs letterlijk bugs (motten, om precies te zijn) geplet in de hardware zouden vinden, en ze zouden loggen! Fast forward naar vandaag, en loggen is een geavanceerde manier geworden om te begrijpen wat er binnen complexe systemen gebeurt.

Hoewel het 'log'-pakket in Go vrij simplistisch is, kan het voldoende zijn voor basisapplicaties. Echter, in de context van moderne gedistribueerde systemen, of wanneer je meer genuanceerde controle over je loguitvoer nodig hebt (zoals verschillende niveaus van ernst), wil je misschien robuustere oplossingen verkennen.

Third-party logbibliotheken zoals `logrus` en `zap` bieden gestructureerd loggen, wat betekent dat je complexe gegevenstypen zoals JSON kunt loggen, wat het makkelijker maakt om logs te interpreteren, vooral in combinatie met logbeheersystemen zoals ELK Stack of Splunk.

Bij het overwegen van de implementatie van een logstrategie, is het ook essentieel om na te denken over prestatie-implicaties. High-performance logbibliotheken zijn geoptimaliseerd om de impact op de applicatiedoorvoer en -latentie te verminderen. Bijvoorbeeld, `zap` pronkt met zijn razendsnelle, lage allocatie ontwerp, wat cruciaal kan zijn voor real-time systemen.

Naast verschillende bibliotheken zijn ook logformaten en -standaarden het vermelden waard. Gestructureerde logformaten zoals JSON kunnen enorm krachtig zijn wanneer ze gebruikt worden in combinatie met logsverwerkingssystemen. Aan de andere kant zijn platte tekstlogs menselijk leesbaar maar moeilijker programmatisch te ontleden.

## Zie Ook
Voor een diepere duik in de logcapaciteiten van Go, kunnen deze bronnen nuttig zijn:

- De Go Blog over loggen: https://blog.golang.org/logging
- `logrus`, een gestructureerde logger voor Go: https://github.com/sirupsen/logrus
- `zap`, een snelle, gestructureerde, geniveleerde logger: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) voor loganalyse: https://www.elastic.co/what-is/elk-stack
- Een vergelijking van Go logbibliotheken: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
