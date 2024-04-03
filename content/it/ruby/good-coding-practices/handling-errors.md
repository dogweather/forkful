---
date: 2024-01-26 00:56:35.279230-07:00
description: "Come fare: Ruby utilizza `begin`, `rescue`, `ensure` ed `end` per gestire\
  \ gli errori. Si racchiude il codice rischioso tra `begin` ed `end`. Se si\u2026"
lastmod: '2024-03-13T22:44:44.058589-06:00'
model: gpt-4-1106-preview
summary: Ruby utilizza `begin`, `rescue`, `ensure` ed `end` per gestire gli errori.
title: Gestione degli errori
weight: 16
---

## Come fare:
Ruby utilizza `begin`, `rescue`, `ensure` ed `end` per gestire gli errori. Si racchiude il codice rischioso tra `begin` ed `end`. Se si verifica un errore, entra in gioco `rescue`.

```Ruby
begin
  # Il codice a rischio va qui.
  puts 10 / 0
rescue ZeroDivisionError => e
  puts "Ops! Non puoi fare questo: #{e.message}"
ensure
  puts "Questo viene sempre eseguito, errore o no."
end
```

Esempio di Output:
```
Ops! Non puoi fare questo: diviso per 0
Questo viene sempre eseguito, errore o no.
```

## Approfondimento
Storicamente, la gestione degli errori nei linguaggi di programmazione ha subito notevoli evoluzioni, con linguaggi antichi che spesso avevano meccanismi grezzi o inesistenti. La gestione delle eccezioni in Ruby è ispirata a linguaggi come Python e Smalltalk.

Alternative al `begin-rescue` in Ruby includono l'uso di `rescue` nelle definizioni dei metodi o l'impiego di `throw` e `catch` per il controllo del flusso non standard, sebbene non siano utilizzati per la tipica gestione degli errori.

Un dettaglio interessante: le eccezioni in Ruby sono oggetti (istanze della classe `Exception` e dei suoi discendenti), quindi è possibile definire classi di errore personalizzate e fare più che semplicemente registrare gli errori — si può trasportare uno stato ricco attraverso il programma per una gestione degli errori più robusta.

## Vedere Anche
- La documentazione Ruby sulle eccezioni e la gestione degli errori: [ruby-doc.org](https://ruby-doc.org/core-3.1.0/doc/syntax/exceptions_rdoc.html)
- Una guida dettagliata sulle migliori pratiche per la gestione degli errori in Ruby: [thoughtbot.com](https://thoughtbot.com/blog/rescue-standarderror-not-exception)
