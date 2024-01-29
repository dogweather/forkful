---
title:                "De lengte van een string vinden"
date:                  2024-01-28T22:00:11.369441-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

De lengte van een tekst vinden betekent het tellen van het aantal tekens dat het bevat. Programmeurs doen dit om invoer te valideren, teksten te splitsen, of gewoon om te weten hoe groot dat stuk tekst is.

## Hoe te:

In Gleam is het uitvinden van een tekstlengte een enkele regel code. Zo doe je het:

```Gleam
import gleam/io
import gleam/string

fn main() {
  let my_string = "Gleam schijnt!"
  let length = string.len(my_string)
  io.println(length) // Output: 12
}
```

Gebruik gewoon de `string.len` functie en geef je tekst door. En voilà! Je krijgt de lengte.

## Diepgaande duik

Er was eens een tijd waarin teksten als magische spreuken waren—moeilijk te hanteren, met elke taal die zijn eigen toverspreuken uitsprak. In talen zoals C loop je handmatig door een karakter array tot je de null-terminator (`'\0'`) tegenkomt om een tekstlengte te vinden. Pijnlijk, toch?

Gleam houdt het echter simpel. Het draait op de BEAM VM—thuisbasis van Erlang en Elixir—die teksten behandelt als een reeks bytes. Dat klopt, bytes, niet tekens. Dit is een belangrijk punt, omdat in Unicode, tekens meer dan één byte kunnen zijn. Gleam teksten zijn UTF-8 gecodeerd, dus een enkel teken kan tussen de 1 tot 4 bytes zijn.

Hier is de adder onder het gras—`string.len` geeft je het aantal bytes, niet het aantal Unicode grafemclusters (wat we vaak denken als tekens). Dus, voor ASCII-teksten (waar elk teken een enkele byte is), is de lengte in bytes gelijk aan het aantal tekens. Voor teksten met emoji of andere multibyte tekens, niet echt.

Voor een snelle oplossing is er momenteel geen ingebouwd alternatief in Gleam. Je moet een bibliotheek invoeren of zelf wat code schrijven als je grafemclusters moet tellen.

## Zie ook

Duik dieper in de behandeling van teksten in Gleam in de officiële documentatie:


En voor een visuele weergave van grafemen, bytes en tekens, bekijk:

- De Unicode Grafemcluster viewer: [https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=](https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=)
