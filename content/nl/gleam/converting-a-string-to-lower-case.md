---
title:                "Een string omzetten naar kleine letters"
date:                  2024-01-28T21:57:46.886608-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het omzetten van een string naar kleine letters betekent het transformeren van alle hoofdletters naar hun kleine letter tegenhangers. Programmeurs doen dit voor consistentie, vooral voor vergelijkingsoperaties of om gebruikersinvoer te standaardiseren.

## Hoe:
In Gleam is stringmanipulatie eenvoudig. Gebruik de functie `string.lowercase` om een string naar kleine letters om te zetten. Hier is een eenvoudig voorbeeld:

```gleam
import gleam/string

pub fn demo() {
  let my_string = "Hello, World!"
  string.lowercase(my_string)
}

// Dit zal uitvoeren: "hello, world!"
```

## Diepere Duik
Voor de uniformiteit van Unicode hadden we ASCII, waar het omzetten naar kleine letters een beetje wiskunde was. Voeg 32 toe aan elke hoofdletter ASCII-karaktercode, en voilà - kleine letters. Met Unicode is het lastiger; de regels zijn complex en taalspecifiek. In Gleam wordt al het zware werk weggenomen, waardoor je een eenvoudige functie krijgt die consistent werkt in verschillende talen.

Alternatieven voor string.lowercase kunnen bestaan uit handmatig over elk karakter in een string heen gaan, controleren of het een hoofdletter is en vervolgens omzetten. Het is weliswaar doenbaar, maar waarom het wiel opnieuw uitvinden?

Onder de motorkap vertrouwt een functie zoals `string.lowercase` waarschijnlijk op de onderliggende Erlang-systeem's unicode-bibliotheken, die robuuste ondersteuning bieden voor casemapping in verschillende talen. Dit is een ongelooflijk complex onderwerp omdat het idee van hoofd- en kleine letters zelfs niet van toepassing is op een groot aantal schrijfsystemen.

## Zie Ook
- Unicode Case Mapping FAQ: [http://www.unicode.org/faq/casemap_charprop.html](http://www.unicode.org/faq/casemap_charprop.html)
- Erlang's unicode-module voor een kijkje onder de motorkap: [https://www.erlang.org/doc/man/unicode.html](https://www.erlang.org/doc/man/unicode.html)
