---
title:                "Een string converteren naar kleine letters"
aliases:
- /nl/c/converting-a-string-to-lower-case/
date:                  2024-02-03T17:54:42.443172-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string converteren naar kleine letters"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het omzetten van een tekenreeks naar kleine letters in C houdt in dat alle hoofdletters in een gegeven tekenreeks worden getransformeerd naar hun overeenkomstige kleine letters. Programmeurs voeren deze bewerking vaak uit om tekstinput te standaardiseren voor vergelijkingen, zoekoperaties of simpelweg voor esthetische consistentie in output.

## Hoe te:

C heeft geen ingebouwde functie voor directe tekenreeksconversie naar kleine letters, in tegenstelling tot sommige hogere programmeertalen. Het proces kan echter eenvoudig worden geïmplementeerd met behulp van de standaardbibliotheekfuncties van C. Hieronder volgt een stap-voor-stap handleiding en een voorbeeld dat illustreert hoe je een tekenreeks omzet naar kleine letters.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Origineel: %s\n", text);

    toLowerCase(text);
    printf("Kleine letters: %s\n", text);

    return 0;
}
```

**Voorbeelduitvoer:**

```
Origineel: Hello, World!
Kleine letters: hello, world!
```

In dit voorbeeld gaat de `toLowerCase` functie door elk teken van de invoertekenreeks en converteert deze naar de overeenkomstige kleine letter met behulp van de `tolower` functie uit `ctype.h`. De wijziging wordt ter plaatse uitgevoerd, waardoor de oorspronkelijke tekenreeks wordt gewijzigd.

## Diepgaande Duik

De `tolower` functie die in het bovenstaande voorbeeld wordt gebruikt, maakt deel uit van de standaardbibliotheek van C, specifiek binnen het `ctype.h` headerbestand. Het functioneert op basis van de huidige locale, maar voor de standaard "C" locale behandelt het de ASCII-tekenreeks waar 'A' tot 'Z' wordt omgezet naar 'a' tot 'z'.

Historisch gezien was de behandeling van tekencodering en hoofdletterconversie in C nauw gekoppeld aan de ASCII-tekenreeks, wat het nut ervan in internationale of gelokaliseerde toepassingen waar tekens buiten de ASCII-set vaak voorkomen, beperkt. Moderne programmeertalen kunnen ingebouwde tekenreekmethoden bieden om hoofdletterconversie uit te voeren met inachtneming van locale en Unicode-tekens, wat C van nature mist.

In scenario's die uitgebreide tekstmanipulatie vereisen, met name met niet-ASCII-tekens, kunnen programmeurs overwegen om bibliotheken te gebruiken die betere ondersteuning bieden voor internationalisering, zoals ICU (International Components for Unicode). Echter, voor de meeste toepassingen die omgaan met ASCII-tekst, is de gedemonstreerde aanpak efficiënt en eenvoudig. Het benadrukt de neiging van C om programmeurs controle te geven over gegevensmanipulatie, zij het met wat meer inspanning vergeleken met hogere programmeertalen.
