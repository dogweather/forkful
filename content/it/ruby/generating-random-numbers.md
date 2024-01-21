---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:50:13.228829-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
La generazione di numeri casuali è fondamentale in programmazione - usiamo questi numeri per tutto dalle simulazioni ai giochi. In Ruby, generare numeri casuali è semplice e serve a introdurre varietà e imprevedibilità nei nostri script.

## How to: (Come fare)
Ruby rende veramente facile generare numeri casuali. Usando `rand` e il modulo `Random`, possiamo ottenere quel pizzico di casualità di cui ogni tanto abbiamo bisogno. Ecco un paio di esempi:

```ruby
# Genera un numero casuale tra 0 e 1
puts rand

# Genera un numero casuale tra 0 e 10
puts rand(11)

# Genera un numero casuale tra 20 e 30
puts 20 + rand(11)

# Usando il modulo Random per un seme (seed) specifico
prng = Random.new(1234)
puts prng.rand(100)
```

Output d'esempio:

```
0.437628907345794
7
29
47
```

## Deep Dive (Approfondimento)
Non molto tempo fa, il casual era affidato a metodi meno affidabili - tipo il movimento del mouse o la tempistica della digitazione. Ma ora, con classi come `Random`, possiamo generare numeri pseudo-casuali di alta qualità in modo ripetibile.

Un'altra alternativa è l'uso di librerie esterne come `SecureRandom`, utile quando la sicurezza è cruciale e ti serve una casualità meno prevedibile.

Non dimenticare, i generatori di numeri casuali in Ruby sono pseudo-casuali; basati su un algoritmo deterministico. Per alcune applicazioni, come la crittografia, questa non è la scelta migliore. Qui potresti voler esplorare opzioni che offrono casualità "vera" (truly random), che sfruttano la fisica quantistica o altri fenomeni naturali.

## See Also (Vedi Anche)
Dai un'occhiata a questi risorsi per una comprensione più approfondita:

- Documentazione ufficiale Ruby su Random: [Ruby-Doc.org - Random](https://ruby-doc.org/core-2.7.0/Random.html)
- Articolo sulla serie sicurezza e `SecureRandom`: [Ruby-Doc.org - SecureRandom](https://ruby-doc.org/stdlib-2.7.0/libdoc/securerandom/rdoc/SecureRandom.html)
- Spiegazione del concetto di pseudo-casualità: [Wikipedia - Generatore di numeri pseudo-casuali](https://it.wikipedia.org/wiki/Generatore_di_numeri_pseudo-casuali)