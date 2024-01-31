---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) è un formato di scambio dati leggero. I programmatori lo usano per via della sua semplicità di lettura e scrittura, oltre al fatto che è facile da analizzare e generare da parte di macchine.

## How to:
Java richiede una libreria per lavorare con JSON, come `org.json`. Ecco un esempio di creazione e lettura di JSON con essa:

```Java
import org.json.JSONObject;

public class JsonDemo {

    public static void main(String[] args) {
        // Creazione di un oggetto JSON
        JSONObject obj = new JSONObject();
        obj.put("nome", "Mario");
        obj.put("età", 30);
        obj.put("abitazione", new JSONObject().put("città", "Roma").put("cap", "00100"));

        // Stampa l'oggetto JSON
        System.out.println(obj.toString());

        // Lettura di un campo dall'oggetto JSON
        String città = obj.getJSONObject("abitazione").getString("città");
        System.out.println("Città: " + città);
    }
}
```

Esempio di output:
```
{"abitazione":{"città":"Roma","cap":"00100"},"età":30,"nome":"Mario"}
Città: Roma
```

## Deep Dive

JSON, nato nei primi anni 2000, è diventato lo standard de facto per lo scambio di dati su Internet, superando formati come XML per la sua leggerezza e facilità di uso. Alternativi a `org.json`, oggi ci sono librerie come Google Gson o Jackson, che offrono funzionalità avanzate e miglior gestione delle prestazioni. Quando si lavora con JSON in Java, è importante gestire eccezioni dovute a dati malformati o cambiamenti inaspettati nella struttura dei dati.

## See Also

- Documentazione ufficiale di `org.json`: https://stleary.github.io/JSON-java/
- Google Gson: https://github.com/google/gson
- Jackson: https://github.com/FasterXML/jackson
- Confronto tra librerie JSON per Java: https://www.baeldung.com/java-json
- Tutorial su JSON e Java: https://www.oracle.com/technical-resources/articles/java/json.html
