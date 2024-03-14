---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:29.214224-07:00
description: "Le espressioni regolari (regex) in Elixir vengono utilizzate per cercare,\
  \ abbinare e manipolare stringhe basate su specifici schemi. I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.074189-06:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) in Elixir vengono utilizzate per cercare,\
  \ abbinare e manipolare stringhe basate su specifici schemi. I programmatori\u2026"
title: Utilizzo delle espressioni regolari
---

{{< edit_this_page >}}

## Cosa & Perché?

Le espressioni regolari (regex) in Elixir vengono utilizzate per cercare, abbinare e manipolare stringhe basate su specifici schemi. I programmatori sfruttano le regex per compiti come la validazione di formati (email, URL), l'analisi dei log o l'estrazione di dati, grazie alla loro efficienza e versatilità nella gestione delle stringhe.

## Come fare:

Elixir utilizza il modulo `Regex`, sfruttando la libreria regex di Erlang, per le operazioni con le regex. Ecco alcuni utilizzi di base:

```elixir
# Abbinamento di uno schema - Restituisce il primo abbinamento
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Output: ["hello"]

# Trovare tutti gli abbinamenti
all_matches = Regex.scan(~r/\d/, "Ci sono 2 mele e 5 arance.")
IO.inspect(all_matches) # Output: [["2"], ["5"]]

# Sostituire parti di una stringa
replaced_string = Regex.replace(~r/\s+/, "Elixir è divertente", "_")
IO.inspect(replaced_string) # Output: "Elixir_è_divertente"
```

Per schemi più complessi e funzionalità, potresti considerare l'uso di librerie di terze parti, sebbene per la maggior parte dei compiti core di abbinamento di stringhe e schemi, il modulo `Regex` integrato in Elixir è piuttosto potente.

Per eseguire un abbinamento che non tiene conto della maiuscola o minuscola, utilizza l'opzione `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Output: ["Hello"]
```

Le espressioni regex possono essere precompilate per l'efficienza quando vengono usate più volte:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Output: ["hello"]
```

Elixir supporta anche le catture nominate, che possono essere molto utili per estrarre parti specifiche di una stringa rendendo il codice più leggibile:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Output: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Questa breve panoramica sottolinea la facilità con cui Elixir gestisce le espressioni regolari, consentendo tecniche potenti di manipolazione delle stringhe ed estrazione dei dati.
