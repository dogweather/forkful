---
title:                "C#: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Il JSON è un formato di dati leggibile e facilmente interpretabile dai computer. L'utilizzo di JSON nella programmazione consente di scambiare dati in modo efficiente tra diverse applicazioni.

## Come fare

Utilizzando il linguaggio di programmazione C#, è possibile creare e leggere dati JSON in modo semplice. Ecco un esempio di come creare un oggetto JSON con due campi:

```C#
var jsonObject = new {
    nome = "Marco",
    età = 30
};
```

Per leggere il valore di un campo specifico, è possibile utilizzare la seguente sintassi:

```C#
Console.WriteLine(jsonObject.nome);
```

L'output dovrebbe essere "Marco". È anche possibile utilizzare il metodo `JsonConvert.SerializeObject()` per convertire un oggetto in una stringa JSON e `JsonConvert.DeserializeObject()` per convertire una stringa JSON in un oggetto.

## Approfondimento

Il formato JSON è molto flessibile e supporta una vasta gamma di tipi di dati, tra cui stringhe, interi, decimali, booleani e anche oggetti annidati. Inoltre, è possibile utilizzare librerie esterne per facilitare la gestione dei dati JSON, come Json.NET sviluppato da James Newton-King.

Inoltre, per lavorare con dati JSON di grandi dimensioni, si consiglia di utilizzare una libreria di analisi e serializzazione JSON ad alte prestazioni, come Jil o UTF8Json.

## Vedi anche

- [Documentazione di Json.NET](https://www.newtonsoft.com/json)
- [Jil - Serializzatore JSON ad alte prestazioni per .NET](https://github.com/kevin-montrose/Jil)
- [Utf8Json - Libreria di analisi e serializzazione JSON ad alte prestazioni per .NET](https://github.com/neuecc/Utf8Json)