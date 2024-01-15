---
title:                "Lavorare con json"
html_title:           "Elm: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Stai pensando di lavorare con JSON in Elm? Non preoccuparti, è più semplice di quanto pensi! JSON (JavaScript Object Notation) è un formato di dati leggibile e facile da comprendere, ed è ampiamente utilizzato per lo scambio di dati tra client e server in applicazioni web moderne.

Se stai sviluppando un'interfaccia utente in Elm e hai bisogno di ottenere dati dal tuo server, imparare a lavorare con JSON è fondamentale. Questo ti permetterà di ricevere e trasformare facilmente i dati in modo da poterli visualizzare nella tua applicazione.

## Come fare

Per prima cosa, dovrai importare il modulo `Json.Decode` nel tuo codice Elm: 

```elm
import Json.Decode exposing (..)
```

Successivamente, dovrai definire un tipo di dati che rappresenti la struttura dei tuoi dati JSON in Elm. Ad esempio, se stai ricevendo un oggetto JSON con le informazioni di un utente, potresti definire il suo tipo come segue:

```elm
type alias User =
  { name : String
  , age : Int
  , email : String
  }
```

Una volta definito il tipo, puoi utilizzare la funzione `decode` del modulo `Json.Decode` per convertire i dati JSON in una valore di tipo `User`:

```elm
decodeUser : Decoder User
decodeUser =
  map3 User
    (field "name" string)
    (field "age" int)
    (field "email" string)

decode : String -> Result String User
decode json =
  decodeString decodeUser json
```

Come puoi vedere, abbiamo utilizzato le funzioni `field` e `map3` per estrarre e mappare i valori desiderati dal JSON. Infine, per ottenere i dati, dobbiamo semplicemente utilizzare la funzione `decode` passando come parametro una stringa contenente il nostro JSON.

```elm
jsonString = """
{
  "name": "John",
  "age": 25,
  "email": "john@example.com"
}

decode jsonString
-- risultato: Ok { name = "John", age = 25, email = "john@example.com" }
```

Se stai lavorando con dati più complessi o vuoi gestire situazioni di errore, è possibile utilizzare altre funzioni del modulo `Json.Decode`come ad esempio `andThen` e `oneOf`.

## Approfondimenti

Lavorare con i dati JSON può essere un'esperienza molto piacevole in Elm grazie alla sua forte tipizzazione e alle funzioni di decodifica. Tuttavia, è importante ricordare di gestire ogni situazione di errore che potrebbe verificarsi durante la decodifica dei dati. Per ulteriori informazioni, puoi consultare la documentazione ufficiale di Elm [qui](https://package.elm-lang.org/packages/elm/json/latest/) ed esplorare altri esempi di codice.

## Vedi anche

- [JSON in Elm - A Beginner's Guide](https://www.freecodecamp.org/news/json-in-elm-a-beginner-s-guide/) 
- [Handling JSON in Elm](https://www.thebookofelm.com/chapter-5/2-hello-json.html) 
- [Elm tutorial: Fetching JSON from a REST API](https://tomwoolf.com/blog/elm-tutorial-fetching-data-apis/)