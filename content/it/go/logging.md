---
title:                "Registrazione delle Attività (Logging)"
date:                  2024-01-26T01:07:42.225390-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione delle Attività (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/logging.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Il logging consiste nel tenere un registro degli eventi, degli stati e dei flussi di dati all'interno di un'app. I programmatori lo fanno per diagnosticare bug, monitorare le prestazioni e tracciare la salute operativa dell'app—rendendolo praticamente l'equivalente software di una scatola nera negli aerei.

## Come fare:
In Go, il logging può essere gestito in vari modi, che vanno dal pacchetto `log` della libreria standard fino a librerie di terze parti come `logrus` e `zap`. Ecco un semplice esempio che utilizza il pacchetto `log` incorporato:

```Go
package main

import (
	"log"
	"os"
)

func main() {
	// Creare un file di log
	logFile, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer logFile.Close()

	// Impostare il file come output del log
	log.SetOutput(logFile)

	// Registrare alcuni eventi
	log.Println("Avvio dell'applicazione...")
	// ... logica dell'applicazione qui ...
	log.Println("Applicazione terminata con successo.")
}
```

Se esegui questo codice, non vedrai alcun output sul terminale perché tutto va in `app.log`. Ecco una sbirciatina a ciò che troveresti all'interno di quel file di log:

```
2023/01/02 15:04:05 Avvio dell'applicazione...
2023/01/02 15:05:01 Applicazione terminata con successo.
```

## Approfondimento
Il logging nella programmazione risale ai primissimi computer, dove gli ingegneri trovavano letteralmente bug (falene, per l'esattezza) schiacciati nell'hardware, e li registravano! Avanzando fino ad oggi, il logging è diventato un modo sofisticato per comprendere cosa sta succedendo all'interno di sistemi complessi.

Sebbene il pacchetto `log` in Go sia abbastanza semplicistico, può essere sufficiente per applicazioni di base. Tuttavia, nel contesto dei moderni sistemi distribuiti, o quando è necessario un controllo più sfumato sull'output dei log (come diversi livelli di gravità), potrebbe essere utile esplorare soluzioni più robuste.

Biblioteche di logging di terze parti come `logrus` e `zap` offrono il logging strutturato, che significa che puoi registrare tipi di dati complessi come JSON, rendendolo più facile da interpretare, specialmente in congiunzione con sistemi di gestione dei log come ELK Stack o Splunk.

Quando si considera l'implementazione di una strategia di logging, è essenziale pensare anche alle implicazioni sulle prestazioni. Le librerie di logging ad alte prestazioni sono ottimizzate per ridurre l'impatto sul throughput e sulla latenza dell'applicazione. Ad esempio, `zap` si vanta del suo design veloce e a basso numero di allocazioni, che può essere cruciale per i sistemi in tempo reale.

Oltre alle varie librerie, sono anche da notare i formati e gli standard di logging. I formati di logging strutturati come JSON possono essere immensamente potenti se utilizzati in congiunzione con sistemi di elaborazione dei log. D'altra parte, i log in testo semplice sono leggibili dall'uomo ma più difficili da analizzare in modo programmatico.

## Vedi Anche
Per approfondire le capacità di logging di Go, queste risorse potrebbero esserti utili:

- Il Blog di Go sul logging: https://blog.golang.org/logging
- `logrus`, un logger strutturato per Go: https://github.com/sirupsen/logrus
- `zap`, un logger veloce, strutturato e livellato: https://github.com/uber-go/zap
- ELK Stack (Elasticsearch, Logstash, Kibana) per l'analisi dei log: https://www.elastic.co/it/what-is/elk-stack
- Un confronto tra le librerie di logging di Go: https://www.loggly.com/blog/benchmarking-5-popular-golang-logging-libraries/
