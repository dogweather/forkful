---
title:    "Ruby: Confrontare due date"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Perché

Comparare due date nel tuo codice può essere utile per una varietà di motivi. Ad esempio, potresti voler verificare se una determinata data è successiva a un'altra o se sono entrambe uguali. Inoltre, la comparazione di date può aiutarti a filtrare o ordinare i dati in base alla loro data. In pratica, ci sono molte situazioni in cui la comparazione di date può semplificare il tuo codice e rendere il tuo programma più efficiente.

## Come fare

Per comparare due date in Ruby, è possibile utilizzare il metodo `.compare` dell'oggetto `Date` fornito dalla libreria standard. Questo metodo restituisce un valore intero che indica la relazione tra le due date. Ecco un esempio di come utilizzarlo:

```ruby
require 'date'

date1 = Date.new(2021, 8, 15)
date2 = Date.new(2021, 8, 20)

if date1 == date2 
  puts "Le date sono uguali"
elsif date1 < date2 
  puts "La prima data è precedente alla seconda data" 
elsif date1 > date2 
  puts "La prima data è successiva alla seconda data" 
end
```
Output:
```
La prima data è precedente alla seconda data
```

Come puoi vedere, abbiamo utilizzato l'operatore di confronto `==` e gli operatori `<` e `>` per confrontare le due date. Tieni presente che il metodo `.compare` restituisce un intero negativo, se la prima data è precedente alla seconda, un intero positivo, se la prima data è successiva alla seconda, e 0 se le due date sono uguali.

## Approfondimento

Perché i metodi `.compare` e gli operatori di confronto funzionano in questo modo? Il motivo è che in Ruby, le date sono semplicemente oggetti che rappresentano un punto nella timeline. Quindi, quando confronti due date, il confronto viene effettuato basandosi sulla loro posizione nella timeline.

Inoltre, puoi anche utilizzare altri metodi utili come `.before?` e `.after?` per semplificare ulteriormente la comparazione di date. E se stai lavorando con date che includono anche l'ora, puoi utilizzare l'oggetto `DateTime` invece di `Date`.

## Vedi anche

- [Documentazione ufficiale Ruby per l'oggetto Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Tutorial su come lavorare con le date in Ruby](https://www.rubyguides.com/2016/08/ruby-date-time-tutorial/)
- [Spiegazione sui metodi .compare, .before?, .after? e altro ancora sulle date in Ruby](https://coderwall.com/p/svy3ug/date-comparisons-in-ruby)