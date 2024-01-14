---
title:    "Arduino: Ta bort tecken som matchar ett mönster"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Varför

Det finns många användbara tillfällen då man kan behöva ta bort tecken som matchar ett visst mönster. Det kan vara för att rensa data eller för att ta bort onödig information från en sträng.

# Hur man gör

Det finns flera olika metoder som kan användas för att ta bort tecken som matchar ett mönster. Ett sätt är att använda sig av funktionen `replace()` som finns inbyggd i Arduino:s String bibliotek. Detta gör det möjligt att söka efter ett visst mönster och ersätta det med ett annat eller bara ta bort det helt.

En annan metod är att använda en for-loop för att gå igenom strängen och kontrollera varje tecken. Om tecknet matchar det valda mönstret så kan det hoppas över eller tas bort från strängen.

```Arduino
// Skapar en sträng med texten "Hej Världen"
String text = "Hej Världen"; 

// Använder funktionen replace() för att ta bort bokstaven "d" från strängen
text.replace("d", "");

// Skriver ut den nya strängen som endast innehåller bokstäverna "Hej Värln"
Serial.println(text);
```

# Fördjupning

När man vill ta bort tecken från en sträng är det viktigt att förstå hur indexering fungerar. I Arduino börjar indexeringen alltid på 0, vilket innebär att det första tecknet i en sträng har indexet 0. Detta kan vara till hjälp när man använder exempelvis en for-loop för att kontrollera varje tecken i en sträng.

En annan viktig aspekt att tänka på är hur olika tecken representeras inom Arduino. Vissa tecken, som t.ex. åäö, representeras med flera byte istället för ett. Detta kan påverka hur man söker efter och tar bort tecken från en sträng.

# Se även

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino replace() function](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Arduino for-loop tutorial](https://www.arduino.cc/reference/en/language/structure/control-structure/for/)
- [Utforskning av teckenbaserade datatyper i Arduino](https://learn.adafruit.com/memories-of-an-arduino/lesson-6-1-character-based-data-types-in-arduino)