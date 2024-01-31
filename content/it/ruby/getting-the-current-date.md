---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:16:42.540522-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente in Ruby significa semplicemente avere accesso alla data di oggi, secondo il sistema del dispositivo in uso. I programmatori utilizzano questa funzione per registrarla, per gestire eventi basati sulla data e per visualizzare informazioni temporali agli utenti.

## Come Fare:
Per ottenere la data corrente in Ruby, useremo la classe `Date`. Assicurati di avere il modulo 'date' incluso nel tuo programma:

```ruby
require 'date'

oggi = Date.today
puts oggi
```

Questo stamperà qualcosa del genere:

```
# => 2023-04-15
```

Se vuoi anche l'ora corrente, utilizza `DateTime`:

```ruby
require 'date'

adesso = DateTime.now
puts adesso
```

L'output sarà:

```
# => 2023-04-15T14:33:00+00:00
```

## Approfondimenti
`Date` e `DateTime` sono classi disponibili in Ruby per gestire date e tempi. Storicamente, Ruby utilizzava la libreria Time per gestire le date, ma essa presenta limitazioni, soprattutto con le date precedenti al 1970 o future al 2038. Date e DateTime forniscono un'alternativa con una maggiore flessibilità. 

Per precisione, `Date` gestisce solo le date mentre `DateTime` include anche informazioni sul tempo. Se non necessiti dell'ora, `Date` è più efficiente. Valuta anche di utilizzare la gem 'active_support/time' per estensioni più potenti delle classi di tempo se lavori su progetti Rails.

Dettagli attuativo: `Date.today` e `DateTime.now` si affidano al fuso orario del sistema del dispositivo in uso. È importante considerare le implicazioni di fuso orario quando si lavora con applicazioni distribuite globalmente.

## Vedi Anche
- Documentazione ufficiale Ruby su `Date`: https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html
- Documentazione ufficiale Ruby su `DateTime`: https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html
- Guida alla gestione dei tempi in Rails (se usi Ruby on Rails): https://guides.rubyonrails.org/active_support_core_extensions.html#time-extensions
