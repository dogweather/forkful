---
title:                "Lavorare con json"
html_title:           "C#: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Perché lavorare con JSON?

Se si lavora con dati strutturati in un programma, è probabile che si sia già incontrato il formato JSON. JSON, acronimo di JavaScript Object Notation, è un formato di testo leggibile e leggero per lo scambio di dati. È ampiamente utilizzato in diverse applicazioni, come ad esempio il web e i servizi API, rendendolo uno strumento importante per gli sviluppatori.

## Come utilizzare JSON in C#

Per utilizzare JSON in C#, è necessario importare lo spazio dei nomi `System.Json` nel proprio progetto. Qui di seguito è riportato un esempio di codice che mostra come convertire un oggetto C# in una stringa JSON e viceversa:

```
using System;
using System.Json;

namespace JsonTutorial
{
    class Program
    {
        static void Main(string[] args)
        {
            // Oggetto C#
            var persona = new
            {
                nome = "Mario",
                cognome = "Rossi",
                età = 30
            };

            // Convertire in stringa JSON
            string jsonString = JsonValue.Parse(persona.ToString()).ToString();

            // Stampa della stringa JSON
            Console.WriteLine(jsonString);

            // Convertire la stringa JSON in oggetto C#
            var personaJson = JsonValue.Parse(jsonString);

            // Stampa dei valori
            Console.WriteLine(personaJson["nome"]);
            Console.WriteLine(personaJson["cognome"]);
            Console.WriteLine(personaJson["età"]);
            
            // Output:
            // {"nome":"Mario","cognome":"Rossi","età":30}
            // Mario
            // Rossi
            // 30
        }
    }
}
```

## Approfondimento su JSON

JSON utilizza un formato di dati gerarchico, che facilita la lettura e l'organizzazione dei dati. È costituito da coppie chiave-valore, dove il valore può essere un altro oggetto JSON, un array o un valore primitivo come una stringa o un numero. JSON è anche facilmente integrabile con molti linguaggi di programmazione, rendendolo uno strumento flessibile per lo scambio di dati tra diverse piattaforme.

## Vedi anche

- [Documentazione ufficiale di JSON](https://www.json.org/json-en.html)
- [Tutorial su JSON in C#](https://www.newtonsoft.com/json/help/html/T_Newtonsoft_Json_Linq_JObject.htm)
- [Introduzione a JSON](https://www.w3schools.com/js/js_json_intro.asp) (in inglese)