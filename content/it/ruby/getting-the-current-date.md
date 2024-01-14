---
title:                "Ruby: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler conoscere la data attuale all'interno del tuo programma Ruby. Potresti aver bisogno di utilizzare la data per creare un file con il formato corretto o per gestire programmi di backup. Inoltre, la conoscenza della data corrente può essere utile nelle applicazioni Web per mostrare informazioni aggiornate ai tuoi utenti.

## Come fare

Per ottenere la data corrente in Ruby, puoi utilizzare il metodo `Time.now`, che restituisce un oggetto `Time` con la data e l'ora attuali.

```ruby
current_date = Time.now
puts current_date
```

Output: `2021-10-27 13:32:47 +0100`

Puoi anche utilizzare il metodo `strftime` per formattare la data in diversi modi:

```ruby
current_date = Time.now
puts current_date.strftime("%d/%m/%Y")
```

Output: `27/10/2021`

## Approfondimento

In Ruby, il tempo è rappresentato come il numero di secondi trascorsi dal 1° gennaio 1970 (conosciuto come "epoch time"). Questa rappresentazione è comune a molti altri linguaggi di programmazione.

Inoltre, puoi anche specificare una timezone specifica utilizzando il metodo `Time.localtime`:

```ruby
current_date = Time.now
puts current_date.localtime("-04:00")
```

Output: `2021-10-27 09:32:47 -0400`

## Vedi anche

- [Time class documentation](https://ruby-doc.org/core-3.0.0/Time.html)
- [Ruby's Time Library](https://www.rubyguides.com/2020/02/ruby-time/)
- [Working with dates and times in Ruby](https://www.twilio.com/blog/working-with-dates-and-times-in-ruby)