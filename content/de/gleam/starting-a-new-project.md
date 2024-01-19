---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# In Angriff Nehmen mit Gleam

## Was & Warum?

Ein neues Projekt zu starten bedeutet, von Grund auf mit einem sauberen Schief zu beginnen. Dies gibt Programmierern die Möglichkeit, eine Anwendung oder ein Softwareprodukt nach ihren spezifischen Bedürfnissen und Visionen zu gestalten.

## Anleitung:

Zum Starten eines neuen Gleam-Projekts folgen Sie den folgenden Schritten im Terminal. 

```
gleam new mein_projekt
cd mein_projekt
rebar3 eunit
```

Nach Ausführung des obigen Codes erhalten Sie eine Ausgabe, die so ähnlich aussieht:

```
===> Verifying dependencies...
===> Compiling mein_projekt
===> Performing EUnit tests...
```

## Vertiefung:

Historisch gesehen stammen Gleam und Erlang, auf dem Gleam aufbaut, von Ericsson ab, wo die Bedürfnis an stabiler und weitrechender Konnektivität groß ist. Alternativ gibt es andere statisch typisierte Sprachen wie Rust und Haskell, die sich jedoch in ihrer Syntax und in den zur Verfügung stehenden Bibliotheken unterscheiden können.

Bevor Sie ein Projekt in Gleam starten, ist es wichtig, die Erlang/OTP-Infrastruktur zu verstehen, da sie den Grundstein für Projekte bildet. Insbesondere sollten Sie mit `rebar3` vertraut sein, dem in Erlang geschriebenen Build-Tool, das beim Starten von Gleam-Projekten verwendet wird.

## Siehe Auch:

Für eine ausführlichere Anleitung zum Starten eines Gleam-Projekts:  
[https://gleam.run/getting-started/](https://gleam.run/getting-started/)

Für Details zur Erlang-Plattform:  
[https://www.erlang.org/](https://www.erlang.org/)

Für weitere Informationen zum `rebar3` Build-Tool:  
[https://www.rebar3.org/](https://www.rebar3.org/)