---
title:                "Elixir: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché
Perché dovresti considerare di lavorare con YAML nella programmazione Elixir? Scoprilo in questo post!

## Come
Per iniziare a lavorare con YAML in Elixir, devi prima importare il modulo `:yaml` utilizzando `use YAML`.
Successivamente, puoi utilizzare il metodo `decode` per convertire un file YAML in un termine Elixir.
Ad esempio:
```
Elixir
%YAML
foo: bar
```
Produce la seguente uscita:
```
Elixir
%{
  "foo" => "bar"
}
```
Puoi anche utilizzare il metodo `encode` per convertire un termine Elixir in file YAML. Ad esempio:
```
Elixir
%{
  "foo" => "bar"
}
```
Produce la seguente uscita:
```
Elixir
%YAML
foo: bar
```

## Deep Dive
Ora che hai visto alcuni esempi su come lavorare con YAML in Elixir, vediamo alcuni concetti più avanzati.
Puoi utilizzare il metodo `decode_stream` per decodificare un file YAML più grande che non può essere caricato interamente in memoria. Ciò è particolarmente utile per la gestione di file YAML di grandi dimensioni.
Inoltre, il modulo YAML supporta anche la serializzazione e il deserializzazione di termini di tipo `:atom`, `:time` e `:datetime`.
Puoi trovare ulteriori informazioni sulla gestione di questi tipi di dati nel modulo `:yaml` nella documentazione ufficiale di Elixir.

## Vedere anche
Se vuoi saperne di più su YAML e Elixir, puoi consultare i seguenti link:
- [Documentazione ufficiale di Elixir sul modulo YAML](https://hexdocs.pm/elixir/YAML.html)
- [Introduzione a YAML nella programmazione Elixir](https://www.sitepoint.com/writing-yaml-elixir/)
- [Tutorial su YAML e Elixir](https://elixircasts.io/yaml-in-elixir)

Grazie per aver letto questo post e speriamo di averti fornito una buona introduzione su come lavorare con YAML in Elixir! Buona programmazione!