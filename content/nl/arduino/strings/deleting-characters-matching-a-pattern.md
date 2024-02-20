---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:35.047113-07:00
description: "Het verwijderen van karakters die overeenkomen met een patroon betekent\
  \ het wegdoen van specifieke volgordes van karakters uit strings\u2014denk aan het\u2026"
lastmod: 2024-02-19 22:05:10.140693
model: gpt-4-0125-preview
summary: "Het verwijderen van karakters die overeenkomen met een patroon betekent\
  \ het wegdoen van specifieke volgordes van karakters uit strings\u2014denk aan het\u2026"
title: Karakters verwijderen die overeenkomen met een patroon
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verwijderen van karakters die overeenkomen met een patroon betekent het wegdoen van specifieke volgordes van karakters uit strings—denk aan het opschonen van data of invoer. Programmeurs doen dit om de informatie te standaardiseren, te vereenvoudigen of te valideren voordat ze deze verwerken.

## Hoe:

Stel, we willen alle numerieke cijfers uit onze string laten vallen. We hebben een string met enkele willekeurige nummers, en we gaan voor een schoon, alleen-letters resultaat.

```Arduino
void setup() {
  Serial.begin(9600);

  // Onze beginstring met nummers
  String stringWithNumbers = "Ar3du1n0 1s aw3som3!";
  String cleanedString = deletePattern(stringWithNumbers, "0123456789");

  // Print de opgeschoonde string
  Serial.println(cleanedString);
}

void loop() {
  // Niets te doen hier
}

String deletePattern(String str, String patroon) {
  for (unsigned int i = 0; i < patroon.length(); i++) {
    str.replace(String(patroon[i]), "");
  }
  return str;
}
```

Als je dit uploadt en uitvoert op je Arduino, zie je de string zonder nummers in de seriële monitor:

```
Arduino is geweldig!
```

## Diepgaande Duik

Het verwijderen van karakters die overeenkomen met een specifiek patroon is geen nieuw concept. Vroege programmeertalen hadden functies om strings te verwerken en te manipuleren. In Arduino, hoewel een high-level functie voor patroonverwijdering niet van nature bestaat, kunnen we onze eigen logica creëren, zoals in de `deletePattern` functie hierboven.

Er zijn alternatieven in andere talen, zoals regex (reguliere expressies) in Python of JavaScript, maar de programmeeromgeving van Arduino is basaler. Het bevat geen regex-functies direct uit de doos, voornamelijk vanwege de beperkte verwerkingscapaciteit en geheugen.

Onder de motorkap itereert onze `deletePattern` functie door onze patroonstring, gebruikt de `String.replace()` methode om het huidige karakter te zoeken en vervangt het met een lege string, waardoor het effectief "verwijderd" wordt uit onze originele string.

## Zie Ook

- Stringmanipulatie met Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Arduino String-referentie: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Meer over stringvervanging: http://www.cplusplus.com/reference/string/string/replace/
