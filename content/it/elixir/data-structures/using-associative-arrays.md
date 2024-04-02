---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:49.491836-07:00
description: "In Elixir, gli array associativi, chiamati Mappe, sono collezioni di\
  \ coppie chiave-valore dove una chiave unica punta a un valore. Sono estremamente\
  \ utili\u2026"
lastmod: '2024-03-13T22:44:43.077169-06:00'
model: gpt-4-0125-preview
summary: "In Elixir, gli array associativi, chiamati Mappe, sono collezioni di coppie\
  \ chiave-valore dove una chiave unica punta a un valore. Sono estremamente utili\u2026"
title: Utilizzo di array associativi
weight: 15
---

## Cosa & Perché?

In Elixir, gli array associativi, chiamati Mappe, sono collezioni di coppie chiave-valore dove una chiave unica punta a un valore. Sono estremamente utili per memorizzare e recuperare dati al volo, rendendo il tuo codice più pulito e la tua vita più semplice.

## Come fare:

Creare una Mappa è semplice. Si usa la sintassi `%{}`, così:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

L'accesso ai valori si ottiene usando le chiavi:

```elixir
IO.puts my_map["name"]
```
Output: `Alex`

Per aggiungere o aggiornare valori, puoi usare la funzione `Map.put/3`:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Output: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Rimuovere le chiavi è altrettanto semplice con `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Output: `%{"location" => "NY", "name" => "Alex"}`

## Approfondimento

Le Mappe in Elixir sono un'evoluzione dei più vecchi tipi di memorizzazione chiave-valore, come gli Hash in Ruby o i Dizionari in Python. Consentono ricerche e inserimenti più efficienti, rendendole la scelta prediletta per la programmazione moderna in Elixir. È degno di nota che prima delle Mappe, Elixir utilizzava i moduli HashDict e Dict, che ora sono deprecati.

Tuttavia, per scenari che richiedono dati ordinati, potresti guardare alle liste di parole chiave in Elixir. Queste sono liste di tuple, efficienti per collezioni più piccole ma non altrettanto performanti per grandi insiemi di dati come le Mappe.

Tieni presente che le Mappe memorizzano le loro chiavi in una struttura "piatta", rendendo l'accesso diretto a valori nidificati un po' complicato. Per una nidificazione profonda, potresti considerare l'accesso strutturato tramite le funzioni `get_in`, `put_in`, `update_in` e `get_and_update_in`, che permettono un approccio più dinamico alla manipolazione di dati nidificati.

In sintesi, mentre le Mappe sono la tua prima scelta per le esigenze di array associativi in Elixir, il linguaggio offre una ricca varietà di strutture dati per ogni scenario, incoraggiandoti a scegliere lo strumento giusto per il lavoro.
