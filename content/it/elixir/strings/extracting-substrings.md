---
date: 2024-01-20 17:45:38.190012-07:00
description: 'Come fare: Elixir rende semplice lavorare con le sottostringhe. Ecco
  qualche esempio pratico.'
lastmod: '2024-03-13T22:44:43.073274-06:00'
model: gpt-4-1106-preview
summary: Elixir rende semplice lavorare con le sottostringhe.
title: Estrazione di sottostringhe
weight: 6
---

## Come fare:
Elixir rende semplice lavorare con le sottostringhe. Ecco qualche esempio pratico:

```elixir
# Ottenere una sottostringa con String.slice/3
stringa = "Ciao, programmatori!"
sottostringa = String.slice(stringa, 0, 4)
IO.puts sottostringa # Output: Ciao

# Usare indici negativi per iniziare dal fondo
sottostringa_fondo = String.slice(stringa, -14, 5)
IO.puts sottostringa_fondo # Output: progr

# Estrarre caratteri con String.at/2
carattere = String.at(stringa, 7)
IO.puts carattere # Output: o

# Dividere una stringa in sottostringhe con String.split/2
parole = String.split(stringa, " ")
IO.inspect parole # Output: ["Ciao,", "programmatori!"]
```

## Approfondimento
Le sottostringhe in Elixir sono gestite in modo efficiente grazie alla rappresentazione binaria delle stringhe. Questo non era così scontato in alcuni linguaggi più vecchi dove lavorare con le stringhe poteva essere meno efficiente. Oggi in Elixir puoi usare funzioni come `String.slice/3` o `String.split/2` per ottenere sottostringhe, ma in passato si ricorreva a manipolazioni a più basso livello. Anche se non sono indispensabili grazie alle funzionalità incorporate di Elixir, esistono librerie di terze parti che offrono ulteriori operazioni sulle sottostringhe, come quelle per espressioni regolari.

## Vedi anche
- [Elixir String Module](https://hexdocs.pm/elixir/String.html) – Documentazione ufficiale che copre tutte le funzioni di manipolazione delle stringhe in Elixir.
- [Elixir Regex Module](https://hexdocs.pm/elixir/Regex.html) – Documentazione ufficiale per lavorare con espressioni regolari in Elixir.
