---
title:                "Arduino: Att hitta längden på en sträng"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Blogg inlägget för Arduino programmering - Hitta längden på en sträng

## Varför

Att hitta längden på en sträng kan vara en användbar funktion när man arbetar med text i en Arduino kod. Det kan hjälpa oss att kontrollera och manipulera olika textsträngar för att få våra projekt att fungera som vi vill.

## Hur man gör

För att hitta längden på en sträng i Arduino kan vi använda oss av en inbyggd funktion som heter `length()`. Denna funktion tar en sträng som argument och returnerar längden på strängen som en integer.

```Arduino
String mittNamn = "Sara";
int längd = mittNamn.length();

//Output: längd = 4
```

Vi kan också använda en for-loop för att gå igenom varje bokstav i strängen och räkna antalet bokstäver. Detta kan vara användbart om vi vill utföra andra åtgärder på varje enskild bokstav.

```Arduino
String mittNamn = "Sara";
int längd = 0;

for (int i = 0; i < mittNamn.length(); i++) {
  längd++;
}

//Output: längd = 4
```

## Fördjupning

För att förstå hur funktionen `length()` fungerar i detalj, är det viktigt att förstå några grundläggande koncept inom programmering och datavetenskap.

En sträng är en samling av tecken som representerar text i en dator. När vi beräknar längden på en sträng så räknar vi egentligen antalet tecken i strängen. Varje tecken tar en viss mängd minnesutrymme, så genom att räkna antalet tecken kan vi få en uppskattning av hur mycket minne som strängen tar upp.

Funktionen `length()` är implementerad i Arduino på ett sätt som gör det mycket effektivt att räkna längden på en sträng. Istället för att gå igenom varje tecken i strängen och räkna, så har den en besparingsteknik som gör att den endast räknar längden en gång och sedan sparar resultatet för framtida användning.

## Se även

För mer information om strängar och hur man arbetar med dem i Arduinoprogrammering, kan du kolla in följande länkar:

- [Arduino Reference - length()](https://www.arduino.cc/reference/en/language/variables/stringobject/length/)
- [W3 Schools - Strings in C](https://www.w3schools.in/c-tutorial/strings/)
- [Programmering för Alla - Strängar i Arduino](https://www.programmeringforalla.se/arduino/str%C3%A4ngar.php)

Tack för att du läste!