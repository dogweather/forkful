---
title:                "Lavorare con il linguaggio yaml"
html_title:           "C#: Lavorare con il linguaggio yaml"
simple_title:         "Lavorare con il linguaggio yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Che cos'è e Perché?
Lavorare con YAML, o "YAML Ain't Markup Language", è un modo utile per organizzare e memorizzare i dati in un formato leggibile per gli umani. I programmatori spesso lo utilizzano per configurare le loro applicazioni o per scrivere script di automazione dei processi.

## Come fare:
Per lavorare con YAML in C#, è necessario utilizzare una libreria che fornisca funzioni di lettura e scrittura per questo formato. Una delle opzioni più comuni è la libreria YamlDotNet.

Ecco un esempio di codice che carica un file YAML e legge il suo contenuto in una variabile:

```C#
using System;
using YamlDotNet.Core;
using YamlDotNet.Serialization;

var input = new StreamReader("file.yml");
var deserializer = new DeserializerBuilder().Build();
var data = deserializer.Deserialize(input);
```

Il codice sopra utilizza il costruttore di Deserializer per creare un'istanza della classe Deserializer, che è responsabile di convertire il file YAML in un oggetto C#.

Per scrivere un file YAML, è possibile utilizzare un processo simile:

```C#
using System;
using YamlDotNet.Serialization;

var data = new MyData
{
    Name = "Prova",
    Text = "Questo è un test"
};

var serializer = new SerializerBuilder().Build();
var yaml = serializer.Serialize(data);
```

## Deep Dive:
Il formato YAML è nato come alternativa più leggibile e facile da usare rispetto al formato JSON, ma non è ancora così ampiamente utilizzato. Sebbene entrambi siano basati sul formato di serie del linguaggio di programmazione Perl, YAML si concentra maggiormente sulla leggibilità per gli umani e supporta una sintassi più complessa.

In alternativa alla libreria YamlDotNet, esistono altre librerie che forniscono funzionalità di lettura e scrittura per il formato YAML in C#, come ad esempio YamlSerializer e YamlCpp.

## Vedi anche:
Per una comprensione più approfondita su come funziona YAML e su come utilizzarlo in C#, è possibile consultare la documentazione ufficiale su [YAML.org](https://yaml.org). Inoltre, puoi trovare esempi e risorse utili sulla pagina di documentazione della libreria YamlDotNet su [GitHub](https://github.com/aaubry/YamlDotNet/wiki).