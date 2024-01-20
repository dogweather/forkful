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

## Cosa & Perché?
Working with JSON è quando i programmatori utilizzano un formato di dati molto comune utilizzato per la trasmissione di dati da un sistema all'altro. È ampiamente utilizzato nei sistemi web e mobile perché è leggero e facile da leggere e scrivere.

## Come fare:
[C# ... ]

Per lavorare con JSON in C#, è necessario utilizzare il pacchetto "Newtonsoft.Json". Qui di seguito è riportato un esempio di come utilizzare questo pacchetto per serializzare un oggetto in una stringa JSON:

```C#
using Newtonsoft.Json;

public class Studente
{
    public string Nome { get; set; }
    public string Cognome { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        Studente studente = new Studente { Nome = "Mario", Cognome = "Rossi" };
        string json = JsonConvert.SerializeObject(studente);
        Console.WriteLine(json);
    }
}
```

L'output sarà qualcosa del genere:

```json
{ "Nome": "Mario", "Cognome": "Rossi" }
```

Per deserializzare una stringa JSON in un oggetto, è possibile utilizzare il metodo `DeserializeObject` della classe `JsonConvert`. Ad esempio, se si ha una stringa JSON come questa: `"{ "Nome": "Mario", "Cognome": "Rossi" }"`, è possibile creare un oggetto `Studente` con i valori corrispondenti utilizzando il seguente codice:

```C#
Studente studente = JsonConvert.DeserializeObject<Studente>(json);
```

## Approfondimento:
JSON (JavaScript Object Notation) è stato inventato da Douglas Crockford nel 2001 ed è diventato uno dei formati di dati più utilizzati nel mondo della programmazione. Alcune alternative popolari a JSON includono XML e YAML. JSON ha guadagnato popolarità grazie alla sua facile leggibilità per il computer e per gli umani, il che lo rende ideale per l'utilizzo nei sistemi web e mobile. I dettagli di implementazione di JSON variano a seconda del linguaggio di programmazione utilizzato, ma in generale utilizza una sintassi molto semplice basata sui costrutti di JavaScript.

## Vedi anche:
- [Documentazione ufficiale di Newtonsoft.Json](https://www.newtonsoft.com/json)
- [Introduzione a JSON su MDN](https://developer.mozilla.org/it/docs/Learn/JavaScript/Objects/JSON)