---
title:                "Elm: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché
YAML è un linguaggio di markup leggibile dall'uomo che consente di strutturare i dati in modo semplice ed efficace. Se stai lavorando su un progetto che richiede la gestione di una grande quantità di informazioni, YAML può essere uno strumento utile per organizzare i tuoi dati in modo chiaro e intuitivo.

## Come fare
Per utilizzare YAML in Elm, è necessario prima installare il pacchetto `sviluppo-jsyaml` utilizzando `npm`.

Una volta installato il pacchetto, è possibile utilizzare le funzioni `encode` e `decode` per convertire i dati in formato YAML e viceversa.

```Elm
import Json.Encode
import Developed.JavaScript.YAML as YAML

-- creazione di un oggetto JSON
myObj = Json.Encode.object
  [ ( "nome", Json.Encode.string "Maria" )
  , ( "età", Json.Encode.int 28 )
  ]

-- encoding in formato YAML
encodedYaml = YAML.encode myObj
```

Oltre alla codifica e decodifica, è possibile utilizzare anche la funzione `decodeEx` per gestire eventuali errori nel formato YAML. Inoltre, è possibile utilizzare i parser per modificare la struttura dei dati YAML in base alle proprie esigenze.

```Elm
-- decodifica di un file YAML
myYaml = "- nome: Maria\n  età: 28\n- nome: Giulia\n  età: 32"
decodedYaml = YAML.decode myYaml

-- utilizzo dei parser per modificare la struttura del file YAML
namesOnly = decodedYaml
    |> YAML.value
    |> YAML.list
    |> List.map (\person -> 
        let name = YAML.parserAt [0, "nome"] YAML.string person
        in Json.Encode.object [("nome", name)])
```

## Approfondimento
Quando si lavora con YAML in Elm, è importante prestare attenzione alla corretta indentazione dei dati. Inoltre, è possibile utilizzare le funzioni `encodePretty` e `decodePretty` per formattare in modo leggibile i dati YAML. 

Per un maggior controllo sul processo di codifica e decodifica, è possibile utilizzare gli encoder e decoder personalizzati.

## Vedi anche
- Documentazione ufficiale di elm-json
- Esempi di utilizzo di YAML in Elm sul repository GitHub
- Tutorial su come utilizzare YAML con Elm su Medium