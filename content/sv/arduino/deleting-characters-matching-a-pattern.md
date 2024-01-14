---
title:    "Arduino: Borttagning av tecken som matchar ett mönster"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Att ta bort tecken som matchar ett mönster är ett vanligt problem som många Arduino-programmerare kan stöta på. Ibland vill man rensa en textsträng från oönskade tecken eller bara filtrera ut specifika delar av en sträng. Oavsett anledningen kan det vara användbart att veta hur man gör detta i dina Arduino-program.

## Hur man gör
För att ta bort tecken som matchar ett visst mönster i en textsträng, kan man använda sig av en kombination av stränghanteringsfunktioner och loopar. Ett enkelt exempel på detta kan se ut som följande:

```Arduino
// Skapa en textsträng med oönskade tecken
String text = "Hej, det är söndag idag!";

// Loopa igenom textsträngen och ersätt tecken som matchar mönstret med en tom sträng
for (int i = 0; i < text.length(); i++) {
  if (text.substring(i, i+1) == "ö") { // byt ut "ö" mot vilket mönster du vill matcha
    text.remove(i, 1);
  }
}

// Skriv ut den nya textsträngen utan oönskade tecken
Serial.println(text); // Output: Hej, det är sndag idag!
```

I exemplet ovan används funktionerna `substring()` för att gå igenom varje enskilt tecken i textsträngen och `remove()` för att ta bort tecknet om det matchar mönstret.

En annan metod för att ta bort tecken som matchar ett mönster är att använda sig av `replace()`-funktionen. Detta kan vara användbart om man vill ersätta de oönskade tecknen med något annat istället för att bara ta bort dem. Exempelvis:

```Arduino
// Skapa en textsträng med oönskade tecken
String text = "Hej, det är söndag idag!";

// Använd replace() för att byta ut alla oönskade tecken mot en *
text.replace("ö", "*"); // byt ut "ö" mot vilket mönster du vill matcha

// Skriv ut den nya textsträngen med de bytta tecknen
Serial.println(text); // Output: Hej, det är s*ndag idag!
```

Det är även möjligt att använda sig av regular expressions (regex) för att ta bort tecken som matchar ett mönster. Detta är en avancerad metod som kan vara mer passande för mer komplexa mönster.

## Djupdykning
Att använda sig av `substring()`- och `remove()`-funktionerna för att ta bort tecken som matchar ett mönster kan vara effektivt, men det finns vissa saker att tänka på. Eftersom en textsträng är en array av tecken, kommer indexet för tecknen ändras när du tar bort ett tecken, vilket kan resultera i felaktig indexering. För att undvika detta kan man loopa baklänges genom textsträngen:

```Arduino
// Skapa en textsträng med oönskade tecken
String text = "Hej, det är söndag idag!";

// Loopa baklänges igenom textsträngen och ta bort tecken som matchar mönstret
for (int i = text.length() - 1; i >= 0; i--) {
  if (text.substring(i, i+1) == "ö") { // byt ut "ö" mot vilket mönster du vill matcha
    text.remove(i, 1);
  }
}
```

Ett annat sätt att undvika felaktig indexering är att använda sig av `charAt()`-funktionen istället för `substring()`. Denna funktion returnerar tecknet vid ett specifikt index istället för en understräng.

En annan viktig sak att tänka på är att `remove()`- och `replace()`-funktionerna endast tar bort det första matchande tecknet. Om mönstret förekommer flera gånger i textsträngen, behöver man använd