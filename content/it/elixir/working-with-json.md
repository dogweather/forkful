---
title:                "Lavorare con json"
html_title:           "Elixir: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se stai leggendo questo articolo, è probabile che tu sia interessato a lavorare con JSON (JavaScript Object Notation) utilizzando il linguaggio di programmazione Elixir. JSON è un formato di dati molto popolare e ampiamente utilizzato per scambiare informazioni tra applicazioni web e mobile. Elixir offre un modo semplice e potente per gestire e manipolare i dati JSON, rendendolo una scelta eccellente per i progetti di sviluppo software. 

## Come fare

Per lavorare con JSON in Elixir, avrai bisogno del modulo di libreria di terze parti chiamato "Jason". Puoi installare questa libreria utilizzando il gestore di pacchetti di Elixir, chiamato Mix. Basta eseguire il seguente comando nella tua shell:

```Elixir
mix deps.get
```

Una volta installata la libreria, puoi iniziare a utilizzarla per analizzare e generare dati JSON. Ad esempio, supponiamo di avere un oggetto mappa in Elixir contenente alcuni dati:

```Elixir
map = %{"name" => "Giulia", "age" => 25}
```

Adesso vogliamo trasformare questo oggetto in una stringa JSON. Possiamo farlo utilizzando la funzione `encode!/1` della libreria di Jason:

```Elixir
Jason.encode!(map)
```

Questo ci darà il seguente output:

```Elixir
"{\"name\":\"Giulia\",\"age\":25}"
```

Per decodificare una stringa JSON in un oggetto Elixir, possiamo utilizzare la funzione `decode!/1` della libreria di Jason. Ad esempio:

```Elixir
Jason.decode!("{\"name\":\"Giulia\",\"age\":25}")
```

Questo restituirà il seguente oggetto mappa in Elixir:

```Elixir
%{"name" => "Giulia", "age" => 25}
```

Puoi anche utilizzare la libreria Jason per analizzare dati JSON più complessi, come array di oggetti e oggetti nidificati. Consulta la documentazione ufficiale della libreria Jason per ulteriori informazioni e esempi.

## Approfondimento

Oltre alle funzioni di base per codificare e decodificare dati JSON, la libreria Jason offre anche una serie di funzioni di utilità per lavorare con dati JSON. Ad esempio, puoi utilizzare le funzioni `get/2` e `get!/2` per estrarre valori da un oggetto JSON in base al loro nome di chiave. Inoltre, puoi utilizzare la funzione `insert/2` per aggiungere nuovi elementi a un oggetto JSON. Consulta la documentazione ufficiale per ulteriori dettagli su queste funzioni e altre funzionalità disponibili.

## Vedi anche

- Documentazione ufficiale della libreria Jason: https://hexdocs.pm/jason/index.html
- Elixir School: JSON Tutorial: https://elixirschool.com/it/lessons/specifics/json/