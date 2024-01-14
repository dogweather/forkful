---
title:                "Java: Kapitalisering av en streng"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å kunne kapitalisere en string er en viktig ferdighet i Java-programmering. Det lar deg endre store bokstaver og små bokstaver i en string, og gir deg mer kontroll over hvordan dataene dine blir presentert.

# Hvordan

Det er flere måter å kapitalisere en string i Java på, avhengig av hva formålet ditt er. Her er to eksempler på metoder for å kapitalisere en string:

```Java
String navn = "jeg heter maria";
// Bruke metoden toUpperCase() for å gjøre alle bokstaver store
String stortNavn = navn.toUpperCase();
System.out.println(stortNavn); // Output: JEG HETER MARIA

// Splitte stringen og bruke metoden substring() for å kapitalisere første bokstav i hvert ord
String[] ord = navn.split(" ");
String kapitalisertNavn = "";
for (String ordet : ord) {
  String førsteBokstav = ordet.substring(0, 1).toUpperCase();
  String restenAvOrdet = ordet.substring(1);
  kapitalisertNavn += førsteBokstav + restenAvOrdet + " ";
}
System.out.println(kapitalisertNavn.trim()); // Output: Jeg Heter Maria
```

# Deep Dive

Når man kapitaliserer en string, er det viktig å være klar over forskjellen mellom metoden toUpperCase() og å splitte og bruke substring(). Metoden toUpperCase() endrer kun bokstavene til store bokstaver, mens den andre metoden gir deg mulighet til å gjøre den første bokstaven stor og resten av ordet små. Det er også viktig å merke seg at både stringen og arrayet er uendret, og det må lagres i en ny variabel for å kunne brukes senere.

# Se også

- [Java String documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Java split() and substring() methods](https://www.w3schools.com/java/java_ref_string.asp)