---
title:                "Fish Shell: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore o un appassionato di informatica, probabilmente hai già sentito parlare di JSON. Ma perché dovresti utilizzarlo e cosa puoi fare con esso? JSON è un formato di dati leggibile per le macchine e semplice per gli umani, molto utilizzato per lo scambio di informazioni tra applicazioni web e server. Se stai lavorando con Fish Shell, imparare a utilizzare JSON può aiutarti a manipolare e gestire meglio i dati all'interno del tuo codice.

## Come fare

Per lavorare con JSON all'interno di Fish Shell, è necessario prima installare il pacchetto fish-json utilizzando il gestore dei pacchetti Oh My Fish. Dopo aver installato il pacchetto, puoi utilizzare il comando ```fish_json``` per visualizzare e manipolare i dati JSON all'interno di una finestra di terminale.

Ecco un esempio di come analizzare un file JSON utilizzando Fish Shell:

```Fish Shell
fish_json parse file.json
```

Questo comando restituirà una rappresentazione a schermo del contenuto del file JSON, suddiviso in elementi individuati dall'interprete di Fish. Puoi anche utilizzare ```fish_json```, senza parametri, per selezionare i campi da un file JSON e visualizzare solo le informazioni necessarie.

## Approfondimento

Il pacchetto fish-json utilizza il programma Python jq per effettuare parsing ed estrazione di dati JSON. È possibile utilizzare jq anche per svolgere attività più complesse, come ricerca all'interno di dati JSON o combinazione di più file JSON in un unico documento. Puoi trovare maggiori informazioni sull'utilizzo di jq e sugli altri comandi disponibili nel suo manuale.

## Vedi anche

- [Guida all'utilizzo di Fish Shell](link1)
- [Documentazione del pacchetto fish-json](link2)
- [Manuale di jq](link3)