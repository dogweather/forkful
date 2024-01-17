---
title:                "Uniendo stringhe"
html_title:           "Ruby: Uniendo stringhe"
simple_title:         "Uniendo stringhe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Concatenare stringhe significa unire più stringhe insieme per formarne una più lunga. I programmatori spesso fanno questo per combinare diverse parole o frasi in un'unica stringa, rendendo più facile la loro manipolazione o visualizzazione.

## Come fare:

```Ruby
puts "Hello " + "world" 
# output: Hello world

first_name = "John"
last_name = "Doe"
full_name = first_name + " " + last_name
puts full_name
# output: John Doe
```

## Approfondimento:

Concatenare stringhe non è una pratica nuova, ma ha una lunga storia nell'informatica. Alcune alternative a questa tecnica includono l'utilizzo di metodi di formattazione delle stringhe o la creazione di array di stringhe per poi unirle. Implementativamente, le stringhe possono essere concatenate tramite l'uso dell'operatore di concatenazione `+` o del metodo `concat`.

## Vedi anche:

Per ulteriori informazioni sull'uso delle stringhe in Ruby, puoi consultare la documentazione ufficiale sulle stringhe (https://ruby-doc.org/core-2.7.1/String.html) o dare uno sguardo ai metodi di formattazione delle stringhe come `sprintf` e `format`.