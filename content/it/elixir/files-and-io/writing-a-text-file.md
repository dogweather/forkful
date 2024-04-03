---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:37.760458-07:00
description: "Come fare: Elixir rende la gestione dei file semplice con moduli incorporati.\
  \ Il modo principale per scrivere su un file \xE8 utilizzando le funzioni\u2026"
lastmod: '2024-03-13T22:44:43.102508-06:00'
model: gpt-4-0125-preview
summary: Elixir rende la gestione dei file semplice con moduli incorporati.
title: Scrivere un file di testo
weight: 24
---

## Come fare:
Elixir rende la gestione dei file semplice con moduli incorporati. Il modo principale per scrivere su un file è utilizzando le funzioni `File.write/2` o `File.write!/2`, dove il primo restituisce una tupla `:ok` o `:error` e il secondo genera un errore in caso di fallimento.

Ecco un esempio semplice:

```elixir
# Scrivere su un file, messaggio semplice
File.write("ciao.txt", "Ciao, Mondo!")

# Quando esegui il codice, crea 'ciao.txt' con "Ciao, Mondo!" come contenuto
```

Per aggiungere contenuti ai file, si utilizza `File.open/3` con le opzioni `[:write, :append]`, poi `IO.binwrite/2` per appendere il contenuto:

```elixir
# Appendere a un file
{:ok, file} = File.open("ciao.txt", [:write, :append])
IO.binwrite(file, "\nAggiungiamo un'altra riga.")
File.close(file)

# Ora 'ciao.txt' include una seconda riga "Aggiungiamo un'altra riga."
```

Se stai lavorando con grandi quantità di dati o hai bisogno di più controllo sul processo di scrittura, potresti usare il modulo `Stream` per scrivere dati sul file in modo pigro:

```elixir
# Scrivere un grande dataset in modo pigro
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Numero: #{&1}\n"))
            |> Stream.take(10)

File.open!("numeri.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# Questo crea 'numeri.txt', scrivendo i numeri da 0 a 9, ognuno su una nuova riga.
```

Per progetti che richiedono una gestione dei file più sofisticata, potresti esplorare librerie di terze parti come `CSV`, che offre funzionalità su misura per la manipolazione di file CSV, ma ricorda, per molti scopi, le capacità incorporate di Elixir sono più che sufficienti.
