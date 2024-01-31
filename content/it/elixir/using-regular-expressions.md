---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Le espressioni regolari (regex) permettono di cercare e manipolare testi con pattern specifici. I programmatori le usano per la loro potenza e flessibilità nella manipolazione delle stringhe e nella validazione dei dati.

## How to: (Come farlo:)
```Elixir
# Cerca se una stringa contiene il pattern "elixir"
Regex.match?(~r/elixir/, "Amo programmare in Elixir!") # Output: true

# Sostituisce tutte le istanze di "gatto" con "cane" 
String.replace("Ho un gatto e un altro gatto.", ~r/gatto/, "cane") # Output: "Ho un cane e un altro cane."

# Estrae tutti i numeri da una stringa
Regex.scan(~r/\d+/, "Ci sono 15 gatti e 30 cani.", capture: :all) # Output: [["15"], ["30"]]
```

## Deep Dive (Approfondimento)

1. **Contesto Storico:** Le espressioni regolari sono nate negli anni '50 e da allora sono diventate uno strumento standard in molti linguaggi di programmazione.
2. **Alternative:** Alcune alternative alle regex includono l'uso di string matching e parsing libraries specifiche per certi tipi di dati, come ad esempio le date o i numeri.
3. **Dettagli Implementativi:** Elixir gestisce le regex tramite il modulo Regex, che a sua volta usa la libreria Erlang `:re`, basata su PCRE (Perl Compatible Regular Expressions).

## See Also (Vedi Anche)

- [Elixir Regex Module](https://hexdocs.pm/elixir/Regex.html) - Documentazione ufficiale del modulo Regex di Elixir.
- [Regex101](https://regex101.com/) - Un ottimo strumento online per testare le vostre espressioni regolari.
- [Learn Regex The Hard Way](https://regex.learncodethehardway.org/) - Una risorsa per apprendere le regex da zero.
- [PCRE documentation](http://www.pcre.org/) - Documentazione su PCRE per comprendere a fondo le espressioni regolari compatibili con Perl.
