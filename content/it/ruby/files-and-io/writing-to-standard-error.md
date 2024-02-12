---
title:                "Scrivere sull'errore standard"
aliases:
- /it/ruby/writing-to-standard-error.md
date:                  2024-02-03T19:34:21.977204-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere sull'errore standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere sull'errore standard (stderr) in Ruby significa indirizzare messaggi di errore o diagnostiche a un flusso di uscita separato, distinto dall'output standard (stdout). I programmatori fanno ciò per differenziare l'output regolare del programma dagli errori e dalle informazioni di debug, facilitando così la diagnosi dei problemi e l'analisi dei log.

## Come fare:
La libreria standard di Ruby offre un modo diretto per scrivere su stderr utilizzando `$stderr` o `STDERR`. Non è necessario utilizzare librerie di terze parti per questa operazione di base.

### Scrivere un semplice messaggio su stderr:
```ruby
$stderr.puts "Errore: File non trovato."
# Oppure equivalente
STDERR.puts "Errore: File non trovato."
```
Esempio di output (su stderr):
```
Errore: File non trovato.
```

### Reindirizzare stderr su un file:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Errore nell'apertura della configurazione."
end
```
Questo frammento di codice reindirizza stderr su un file chiamato `error.log`, e tutti gli errori scritti successivamente verranno inviati lì fino a quando il programma non reimposta il reindirizzamento di stderr o termina.

### Utilizzare stderr con la gestione delle eccezioni:
```ruby
begin
  # Simulazione di un'operazione che potrebbe fallire, ad es., aprire un file
  File.open('file_inesistente.txt')
rescue Exception => e
  STDERR.puts "Eccezione generata: #{e.message}"
end
```
Esempio di output (su stderr):
```
Eccezione generata: Nessun file o directory @ rb_sysopen - file_inesistente.txt
```

Sebbene i metodi integrati in Ruby per scrivere su stderr siano sufficienti per molte applicazioni, per esigenze di logging più complesse, si potrebbe considerare l'uso della libreria standard `logger` o di gemme esterne come `Log4r`. Questi forniscono meccanismi di logging configurabili, inclusi livelli di severità, formattazione e la capacità di scrivere su varie uscite, inclusi file, email e altro.
