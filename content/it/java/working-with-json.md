---
title:                "Lavorare con json"
html_title:           "Java: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-json.md"
---

{{< edit_this_page >}}

## Che cos'è e Perché?

Lavorare con JSON, o JavaScript Object Notation, è un modo comune per trasferire dati tra applicazioni e servizi web. JSON è un formato di testo semplice e leggibile per l'archiviazione e lo scambio di dati, ed è diventato molto popolare tra i programmatori grazie alla sua semplicità e compatibilità con molti linguaggi di programmazione.

## Come:

Ecco un esempio di come lavorare con JSON in Java utilizzando la libreria GSON:

```Java
// importare la libreria GSON
import com.google.gson.*;

// creare un oggetto JSON
JsonObject obj = new JsonObject();

// aggiungere un valore
obj.addProperty("nome", "Maria");
obj.addProperty("eta", 25);

// trasformare l'oggetto in stringa JSON
String jsonString = obj.toString();

// stampare il risultato
System.out.println(jsonString);
```

**Output:**

`{"nome":"Maria","eta":25}`


## Deep Dive:

JSON è stato originariamente creato nel 2001 da Douglas Crockford ed è diventato uno standard di fatto per la comunicazione tra client e server. Sebbene sia molto popolare, ci sono delle alternative a JSON, tra cui XML e YAML. Tuttavia, JSON rimane la scelta preferita per molte applicazioni grazie alla sua semplicità ed efficienza.

Per implementare l'utilizzo di JSON in Java, è possibile utilizzare la libreria GSON, che fornisce metodi per creare, analizzare e manipolare oggetti JSON. È importante anche conoscere la struttura di base di JSON, che comprende coppie di chiavi e valori separati da due punti.

## Vedi anche:

- [Documentation for GSON](https://github.com/google/gson)
- [JSON official website](https://www.json.org/)
- [Difference between JSON and XML](https://www.geeksforgeeks.org/difference-between-json-and-xml/)