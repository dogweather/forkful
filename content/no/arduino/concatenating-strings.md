---
title:                "Arduino: Kombinering av strenger"
simple_title:         "Kombinering av strenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor

Mange ganger i Arduino programmering, vil du trenge å kombinere flere tekststrenger sammen for å skape en komplett utgang. Dette kalles "concatenation" og er en nyttig teknikk for å lage dynamiske meldinger og datautgang. Så hvis du vil ta dine Arduino prosjekter til neste nivå, er det viktig å lære hvordan du kan gjøre dette.

# How To

Kodingen for å kombinere strenger i Arduino er veldig enkel. Først må du definere to tekststrenger som du vil kombinere, for eksempel:

```Arduino
String navn = "Arne";
String beskjed = "Velkommen til mitt prosjekt!";
```

Deretter kan du kombinere dem ved hjelp av "+" -operatøren og lagre det kombinerte resultatet i en ny variabel som denne:

```Arduino
String fullBeskjed = navn + beskjed;
```

Du kan også legge til flere strenger ved å fortsette å bruke "+" -operatøren, for eksempel:

```Arduino
String tid = "Klokken er nå: ";
int klokkeslett = 15;
fullBeskjed = tid + klokkeslett + "åtte" + "på kvelden";
```

Til slutt, kan du skrive ut den kombinerte strengen ved hjelp av `Serial.println()` -funksjonen:

```Arduino
Serial.println(fullBeskjed);
```

Outputen vil da være:

```Arduino
Klokken er nå: 15:08 på kvelden
```

# Deep Dive

Nå som du vet hvordan du kan kombinere strenger i Arduino, er det viktig å forstå noen av begrensningene. Først er det viktig å merke seg at det bare er mulig å kombinere datatyper av samme type. For eksempel kan du ikke kombinere en `String` og en `int`, men du kan konvertere tallet til en tekststreng før du kombinerer dem.

I tillegg er det viktig å være oppmerksom på minnebruken når du kombinerer strenger. Hver gang du kombinerer to strenger, blir en ny strengobjekt opprettet i minnet. Dette kan forårsake problemer hvis du kombinerer veldig lange strenger eller hvis du kombinerer strenger i en løkke som kjører mange ganger.

Et annet viktig poeng å huske på er at `String` -klassen i Arduino ikke støtter alle funksjoner som finnes i standard C ++ -biblioteker. Så hvis du prøver å bruke en funksjon som ikke er tilgjengelig i `String` -klassen, kan du få en feilmelding.

# Se også

- [Arduino String klasse referanse](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [W3Schools om string concatenation i Arduino](https://www.w3schools.com/java/java_strings_concatenation.asp)
- [Arduino Forum om string concatenation og minnebruk](https://forum.arduino.cc/index.php?topic=93164.0)