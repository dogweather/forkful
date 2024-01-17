---
title:                "Lavorare con JSON"
html_title:           "Elixir: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Lavorare con JSON è un'attività comune per i programmatori Elixir. JSON (JavaScript Object Notation) è un formato di dati leggibile sia per gli umani che per le macchine, utilizzato per la memorizzazione e lo scambio di dati. Questo è utile per creare applicazioni web dinamiche e interattive.

## Come fare:

Per lavorare con JSON in Elixir, è necessario utilizzare il modulo "Poison". Ecco un esempio di come convertire un oggetto Elixir in formato JSON e viceversa:

```Elixir
# Converte un oggetto Elixir in formato JSON
Poison.encode!(%{nome: "Maria", età: 28})
# Output: "{\"nome\": \"Maria\", \"età\": 28}"

# Converte un oggetto JSON in formato Elixir
Poison.decode!(~s({"nome": "Luca", "età": 32}))
# Output: %{"nome" => "Luca", "età" => 32} 
```

## Approfondimento:

JSON è stato introdotto nel 2005 e da allora è diventato il formato dati più popolare per il web. In alternativa, i programmatori possono utilizzare anche altri formati come YAML o XML, ma JSON è preferito per la sua semplicità e leggibilità.

Il modulo "Poison" utilizza il parser JSON di C e lo integra con Elixir. Se si desidera una soluzione puramente Elixir, si può utilizzare il modulo "Jason", che utilizza un parser scritto in Elixir.

## Vedi anche:

- [Documentazione ufficiale di Poison](https://hexdocs.pm/poison/readme.html)
- [Documentazione ufficiale di Jason](https://hexdocs.pm/jason/README.html)
- [Introduzione a JSON su MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/JSON)