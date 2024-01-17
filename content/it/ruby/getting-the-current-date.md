---
title:                "Ottenere la data corrente."
html_title:           "Ruby: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

Cosa e perché?
Ottenere la data attuale è un'operazione comune mentre si scrive un programma Ruby. Ciò consente ai programmatori di aggiungere stampe di data dinamiche, utilizzare la data nelle operazioni matematiche o semplicemente tenere traccia del tempo.

Come fare:
È possibile ottenere la data attuale utilizzando il metodo "now" nell'oggetto Time. Ad esempio, per stampare la data attuale, è sufficiente utilizzare il seguente codice:

```Ruby
data_attuale = Time.now
puts data_attuale
```

Questo dovrebbe stampare qualcosa del genere: "2022-01-19 12:00:00".

Se si vuole personalizzare il formato della data, è possibile utilizzare il metodo "strftime" e specificare il formato desiderato come parametro. Ad esempio, il seguente codice stamperebbe solo la data nel formato gg/mm/aaaa:

```Ruby
data_attuale.strftime("%d/%m/%Y")
```

Deep Dive:
Ottenere la data attuale è un'operazione relativamente semplice grazie all'oggetto Time di Ruby. Tuttavia, esistono anche altre alternative per ottenere la data, come l'utilizzo di librerie esterne come la gemma "date", che offre funzionalità più avanzate per la manipolazione delle date.

Inoltre, è importante notare che la data attuale viene ottenuta in base alle impostazioni del sistema in cui viene eseguito il programma. Ad esempio, se il sistema è impostato su un fuso orario diverso, la data attuale verrà restituita in base a quel fuso orario.

Vedi anche:
Per ulteriori informazioni su come ottenere la data attuale in Ruby, puoi consultare la documentazione ufficiale di Ruby o esplorare risorse online come Stack Overflow.