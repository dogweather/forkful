---
title:                "Scaricare una pagina web"
html_title:           "Elixir: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché scaricare una pagina web

Scaricare una pagina web può essere un'operazione utile in diverse situazioni, ad esempio per l'estrazione di dati o per l'analisi di un sito. Con l'utilizzo di Elixir, questa operazione può essere automatizzata in modo efficace e veloce.

## Come fare

Per iniziare a scaricare una pagina web utilizzando Elixir, è necessario installare e importare la libreria `HTTPoison` nel codice:

```
Elixir
defp deps do
  [{:httpoison, "~> 1.0"}]
end
```

Successivamente, è possibile utilizzare la funzione `HTTPoison.get/2` specificando l'URL della pagina da scaricare e una lista di opzioni. Ad esempio, per scaricare la pagina del sito "google.com" si può utilizzare questo codice:

```
Elixir
response = HTTPoison.get("https://google.com", [])
```

Il risultato di questa chiamata sarà un'ottima struct contenente i dati della pagina scaricata, come l'header, il body e lo status code.

Per facilitare l'utilizzo di questa libreria, è possibile utilizzare anche un wrapper come `HTTPotion` o `HttPoison` che offrono una sintassi più semplice e intuitiva. Ad esempio, con `HTTPotion` il codice per scaricare la pagina di Google diventa semplicemente:

```
Elixir
response = HTTPotion.get("https://google.com")
```

Inoltre, con queste librerie è possibile specificare ulteriori opzioni per la richiesta, come ad esempio l'utilizzo di proxy o di autenticazione.

## Approfondimento

Il processo di download di una pagina web non è solo una semplice operazione di scaricamento dei dati, ma è anche influenzato da una serie di aspetti come la gestione degli errori, l'efficienza nel download e la gestione delle richieste concorrenti.

Con l'utilizzo di librerie come `HTTPoison`, è possibile gestire efficacemente tutti questi aspetti senza dover scrivere codice ridondante. Inoltre, grazie all'utilizzo di Elixir e della programmazione funzionale, è possibile sfruttare al massimo il paradigma di concorrenza e parallelismo per aumentare le prestazioni del download di una pagina web.

## Vedi anche

- [Documentazione di HTTPoison](https://hexdocs.pm/httpoison/)
- [Documentazione di HTTPotion](https://hexdocs.pm/httpotion/)
- [Documentazione di HttPoison](https://hexdocs.pm/hackney/)