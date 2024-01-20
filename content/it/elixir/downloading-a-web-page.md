---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Scaricare una pagina web significa letteralmente prelevare dati da un sito web e memorizzarli nel tuo dispositivo locale. I programmatori lo fanno, per esempio, per analizzare il codice HTML o per creare snapshot del sito per un utilizzo successivo.

## Come Fare:

Elixir rende tutto molto più semplice con la sua libreria HTTPoison. Ecco un esempio di come scaricare una pagina web.

```Elixir
defmodule Download do
  def get_page(url) do
    case HTTPoison.get(url) do
      {:ok, response} ->
        File.write("web_page.html", response.body)
      {:error, reason} ->
        IO.puts("Errore: #{reason}")
    end
  end
end

Download.get_page("http://example.com")
```
Quando esegui il codice sopra, verrà scaricata la pagina di 'example.com' e salvata come "web_page.html".

## Approfondimento

Scaricare pagine web è una pratica piuttosto comune sin dalle origini del web. Ci sono diverse librerie e strumenti disponibili per diversi linguaggi di programmazione. L'uso della libreria HTTPoison in Elixir è un'ottima opzione, ma ci sono anche alternative come Tesla o Finch.

L'implementazione interna di queste operazioni esamina il protocollo HTTP nella sua interezza. Poiché le pagine web sono fornite tramite HTTP, queste librerie utilizzano questo protocollo per richiedere e ricevere dati.

## Vedere Anche

1. [HTTPoison in Github](https://github.com/edgurgel/httpoison)
2. [Documentazione Elixir](https://elixir-lang.org/docs.html)
3. [Tesla, un'altra libreria HTTP client per Elixir](https://github.com/teamon/tesla)
4. [Finch, un altro client HTTP per Elixir](https://github.com/keathley/finch)
5. [Corso online gratuito su Elixir](https://elixirschool.com/en/)