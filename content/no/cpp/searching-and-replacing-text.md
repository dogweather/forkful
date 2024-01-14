---
title:    "C++: Søke og erstatte tekst"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Hvorfor?

De fleste programmerere har opplevd å måtte endre eller erstatte store mengder tekst i koden sin. Dette kan være en svært tidkrevende og kjedelig oppgave å gjøre manuelt. Derfor er det viktig å lære hvordan man kan søke og erstatte tekst ved hjelp av C++ programmering.

# Hvordan?

For å søke og erstatte tekst i C++ bruker vi en funksjon kalt `replace()`. Denne funksjonen tar imot tre parametere: en start-posisjon, en lengde og en ny tekst som skal erstatte det gamle. Vi kan også bruke funksjonen `find()` for å finne en spesifikk tekst og deretter bruke `replace()` for å erstatte den.

```
// Eksempel på bruk av replace()
string tekst = "Hei, mitt navn er Jens!";
tekst.replace(18, 4, "Ole"); // Erstatter "Jens" med "Ole"
cout << tekst << endl; // Output: "Hei, mitt navn er Ole!"

// Eksempel på bruk av find() og replace()
string navn = "Mari, Sara, Kristian, Markus";
int startPos = navn.find("Kristian"); // Finner posisjonen til "Kristian"
navn.replace(startPos, 8, "Eirik"); // Erstatter "Kristian" med "Eirik"
cout << navn << endl; // Output: "Mari, Sara, Eirik, Markus"
```

# Dypdykk

I tillegg til `replace()` og `find()` finnes det også andre måter å søke og erstatte tekst i C++. For eksempel kan vi bruke funksjonen `regex_replace()` fra `<regex>` biblioteket for å erstatte tekst ved hjelp av et regulært uttrykk. Dette gir oss mer fleksibilitet og muligheten til å erstatte tekst basert på et mønster, ikke bare en eksakt streng.

```
// Eksempel på bruk av regex_replace()
string uttrykk = "I dag er det 01.01.2021, men i morgen er det 02.01.2021.";
std::regex dato("(\\d{2}\\.\\d{2}\\.\\d{4})"); // Definerer et uttrykk for datoer
cout << std::regex_replace(uttrykk, dato, "neste dag") << endl;
// Output: "I dag er det neste dag, men i morgen er det neste dag."
```

# Se også

- C++ strings: https://www.learncpp.com/cpp-tutorial/610-cpp-strings/
- C++ regular expressions: https://www.regular-expressions.info/posix.html
- C++ string functions: https://www.cplusplus.com/reference/string/string/