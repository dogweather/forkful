---
title:                "Elixir: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se vi state chiedendo perché dovreste lavorare con JSON in Elixir, la risposta è piuttosto semplice: JSON è un formato di dati molto popolare e ampiamente utilizzato nel mondo della programmazione. Sapere come manipolare e gestire i dati JSON vi permetterà di creare applicazioni potenti e interoperabili.

## Come fare

Elixir rende molto facile e intuitivo lavorare con JSON. Per cominciare, è necessario importare il modulo `Poison` che offre funzioni per la codifica e la decodifica dei dati JSON.

```Elixir
# importazione del modulo Poison
import Poison
```

Per convertire un dato Elixir in JSON, è possibile utilizzare la funzione `encode!`.

```Elixir
# definizione di un dato Elixir
data = %{nome: "Maria", età: 30}

# conversione in JSON
json = encode!(data)

# output
"{\"nome\":\"Maria\",\"età\":30}"
```

Per decodificare un dato JSON in Elixir, si può invece utilizzare la funzione `decode!`.

```Elixir
# definizione di un dato JSON
json = "{\"nome\":\"Maria\",\"età\":30}"

# conversione in Elixir
data = decode!(json)

# output
%{nome: "Maria", età: 30}
```

## Approfondimento

Mentre le funzioni `encode!` e `decode!` sono perfette per la maggior parte delle situazioni, è possibile utilizzare il modulo `Poison.Optimized` per una maggiore velocità e performance. Inoltre, è possibile personalizzare il modo in cui i dati JSON vengono codificati e decodificati utilizzando le opzioni del modulo `Poison.Encoder` e `Poison.Decoder`.

## Vedi anche

Se desiderate approfondire ulteriormente il vostro studio di JSON in Elixir, qui di seguito trovate una lista di risorse utili:

- [Documentazione ufficiale su Poison](https://hexdocs.pm/poison/Poison.html)
- [Esempi di codice su come lavorare con JSON in Elixir](https://medium.com/@kenmazaika/json-handling-in-elixir-37222fb07324)
- [Video tutorial su come utilizzare JSON in Elixir](https://www.youtube.com/watch?v=UeeBY5izqH0)

Grazie per aver letto questo articolo sulle basi di come lavorare con JSON in Elixir. Speriamo vi sia stato utile!