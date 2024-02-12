---
title:                "Registrazione Eventi (Logging)"
aliases:
- /it/cpp/logging.md
date:                  2024-01-26T01:01:01.036047-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione Eventi (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/logging.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Nel contesto della programmazione, il logging è il processo di registrazione di eventi, stati e informazioni in un file o in un altro mezzo di output. I programmatori effettuano il logging per tenere traccia di ciò che sta succedendo nelle loro applicazioni, per debuggare problemi e per monitorare le prestazioni in vista di future analisi e ottimizzazioni.

## Come fare:
Diciamo che stai lavorando su una macchina Linux e vuoi lanciare i tuoi log in un file con il buon vecchio C++. Vorrai includere le librerie `<iostream>` e `<fstream>` per effettuare operazioni su file. Ecco un esempio rapido:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Apertura in modalità append

    if (!logFile.is_open()) {
        std::cerr << "Si è verificato un problema nell'apertura del file di log!" << std::endl;
        return 1;
    }

    logFile << "Applicazione avviata" << std::endl;
  
    // ... da qualche parte nella logica della tua app
    logFile << "Si è verificato un evento importante" << std::endl;

    // Non dimenticare di chiudere il tuo flusso di file
    logFile.close();

    return 0;
}
```

Se esegui il comando `tail -f appLog.txt`, dovresti vedere:

```
Applicazione avviata
Si è verificato un evento importante
```

Fantastico, hai un registro degli eventi con timestamp!

## Approfondimenti
Il logging è antico quanto l'informatica stessa, con radici che risalgono ai segni letterali su carta per tracciare ciò che i computer antichi stavano facendo. Nell'era moderna, si tratta tutto di sofisticate soluzioni software. Hai il logging diretto su file, come l'esempio rapido ed efficace sopra, o potresti indulgere in un framework di logging più sofisticato, come Log4cpp o Boost.Log nel mondo del C++; questi tosti offrono livelli di logging, controllo dei formati e altro ancora.

Parlando di livelli, le migliori pratiche di logging includono l'uso di vari livelli di gravità—informazioni, debug, avviso, errore, fatale—così puoi filtrare il rumore quando stai cercando di schiacciare i bug o capire perché la tua app si comporta come un adolescente lunatico.

Riguardo alle prestazioni, non essere negligente con i tuoi log. Un logging eccessivo può trasformare la tua app velocissima in una maratona di lumache, appesantire i sistemi di file o addirittura costarti soldi in termini di spese di archiviazione se sei basato sul cloud. Trovare il giusto equilibrio è la chiave: registra ciò di cui hai bisogno, e niente di più.

## Vedi Anche
Per quelli di voi che vogliono andare oltre con le pratiche di logging, date un'occhiata a:

- La [Biblioteca Boost.Log](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) per alcune funzioni di logging di alto livello.
- [La biblioteca glog di Google](https://github.com/google/glog) se sei interessato a quello che i cuochi del gigante tecnologico usano per registrare le loro app.
- [La biblioteca Log4cpp](http://log4cpp.sourceforge.net/) per un meccanismo di logging configurabile.

E per un po' di lettura di sfondo sui motivi e i modi del logging, approfondisci:

- Questo thread di Stack Overflow sulle [migliori pratiche di logging](https://stackoverflow.com/questions/783956/logging-best-practices) ti offrirà un approfondimento approfondito e recensito dai pari sull'argomento.
