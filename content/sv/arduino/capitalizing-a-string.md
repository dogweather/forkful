---
title:    "Arduino: Att Göra En Sträng Kapital"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att sätta versaler på en sträng kan vara användbart för att göra texten mer läsbar eller för att matcha en specifik formatering. Det kan också vara en nödvändig process för att i sin kod hantera olika typer av data.

## Så här gör du

För att sätta versaler på en sträng i Arduino, kan du använda en inbyggd funktion som heter "toUpperCase ()". Denna funktion tar en sträng som inmatning och returnerar en ny sträng med alla versaler.

```Arduino
String str = "hej! detta är en sträng";

// använd toUpperCase () för att sätta versaler på strängen
String result = str.toUpperCase();

// skriver ut det nya värdet
Serial.println(result);

// Output: HEJ! DETTA ÄR EN STRÄNG
```

För att säkerställa att funktionen fungerar korrekt kan du också använda en loop för att loopa genom varje tecken i strängen och omvandla det till versaler.

```Arduino
String str = "hej! detta är en sträng";

// loopar igenom varje tecken i strängen
for (int i = 0; i < str.length(); i++) {
  // omvandlar tecknet till versal och skriver ut det
  Serial.print(str.charAt(i).toUpperCase());
}

// Output: HEJ! DETTA ÄR EN STRÄNG
```

## Fördjupning

Att sätta versaler på en sträng kan verka enkelt, men det finns vissa saker att tänka på för att säkerställa att din kod fungerar korrekt. En av de största utmaningarna är att hantera teckenkodning. Eftersom olika språk har olika bokstäver och symboler, är det viktigt att förstå vilken teckenkodning som används för att korrekt omvandla tecken till versaler.

Du kan också använda andra inbyggda funktioner som "toLowerCase ()" för att omvandla strängen till gemener.

## Se även

- [Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [toUpperCase ()](https://www.arduino.cc/reference/en/language/functions/string/bytecast/ascii/touppercase/)
- [CharAt ()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/charat/)
- [toLowerCase ()](https://www.arduino.cc/reference/en/language/functions/string/bytecast/ascii/tolowercase/)