---
title:                "Analisi dell'html"
html_title:           "Arduino: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Il parsing di HTML è il processo attraverso il quale un programmatore estrae informazioni strutturate da una pagina web. Questo può essere utile per ottenere dati specifici dai siti web, come ad esempio l'ultimo punteggio di una partita o il prezzo di un prodotto. 

## Come fare:

```
ArduinoHttpClient client;

if (client.get("http://www.example.com")) {
    while (client.available()) {
        char c = client.read();
        Serial.write(c);
    }
}
```

Una volta impostato l'oggetto ```ArduinoHttpClient```, è possibile utilizzare il metodo ```get()``` per effettuare una richiesta HTTP e recuperare il contenuto della pagina web. Il metodo restituirà un valore booleano, ```true``` se la richiesta è andata a buon fine, ```false``` altrimenti. Nel primo caso, si può utilizzare un ciclo ```while``` per leggere il contenuto carattere per carattere e stamparlo sulla porta seriale con il metodo ```Serial.write()```.

## Approfondimento:

Il parsing di HTML è una tecnica molto comune nel web scraping, ovvero l'estrazione di dati da un sito web. Ci sono diverse alternative per effettuare questa operazione, tra cui l'utilizzo di librerie specializzate come ```ArduinoJSON``` o la creazione di espressioni regolari per individuare i dati desiderati.

Il metodo mostrato in questo articolo è adatto soprattutto per pagine web statiche, mentre per quelle dinamiche potrebbe essere necessario utilizzare un approccio diverso, come ad esempio l'automazione di un browser attraverso lo strumento Selenium.

## Vedi anche:

- [Documentazione ufficiale di ArduinoHttpClient](https://www.arduino.cc/en/Reference/ArduinoHttpClient)
- [Web scraping con Arduino e Python](https://randomnerdtutorials.com/arduino-automatic-web-scraping/)
- [Esempio di parsing di HTML con ESP8266 e libreria WiFiClient](https://gist.github.com/sean-h/6ab8fe2c6cute2f4f674)