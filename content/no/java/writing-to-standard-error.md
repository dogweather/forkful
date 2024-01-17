---
title:                "Skriver til standardfeil"
html_title:           "Java: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Hva & hvorfor?
Skrive til standardfeil i Java er en måte å utskrive feilmeldinger og annen informasjon til en annen strøm enn standardutgang. Dette er nyttig for å identifisere og løse feil i koden din, samt å gi nyttig informasjon til brukeren av programmet ditt.

Slik gjør du det:
```
try {
    // Ditt kode her
} catch (Exception e) {
    System.err.println("Det har oppstått en feil: " + e.getMessage());
}
```
Resultatet vil bli skrevet ut i terminalen som: "Det har oppstått en feil: <melding>".

Dypdykk:
Skriving til standardfeil har vært en vanlig praksis i programmering siden de tidligste versjonene av Java. Det er en pålitelig måte å kommunisere feil på og har blitt adoptert av andre programmeringsspråk som C++ og Python. Mens noen foretrekker å bruke ```System.out```, er det en god praksis å bruke ```System.err``` for å skille mellom generell utgang og feilmeldinger.

Ser også:
For mer informasjon om utskrift til standardfeil i Java, se Java dokumentasjonen: https://docs.oracle.com/javase/tutorial/essential/io/notification.html.