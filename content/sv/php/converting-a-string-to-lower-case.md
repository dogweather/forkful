---
title:                "Omvandla en sträng till gemener"
html_title:           "PHP: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till små bokstäver är en vanlig programmeringsteknik som används för att standardisera input och jämföra strängar på ett mer flexibelt sätt. Genom att konvertera alla tecken till små bokstäver kan man enkelt utesluta eventuella skillnader i bokstavsstorlek mellan strängar och fokusera på innehållet istället.

## Hur gör man?

```PHP
$string = "ExempelSTRÄNG";
echo strtolower($string);
```

Detta kommer att ge följande output:

```PHP
exempelsträng
```

Som du kan se har nu alla bokstäver konverterats till små bokstäver vilket gör det enklare att jämföra denna sträng med en annan som också har konverterats på samma sätt.

## Grundlig genomgång

Att konvertera en sträng till små bokstäver är ett vanligt tillvägagångssätt för att undvika eventuella skillnader i bokstavsstorlek när man jämför strängar. Det grundläggande systemet för teckenkodning, ASCII, använde sig endast av stora bokstäver. Det var först med utvecklingen av Unicode som både små och stora bokstäver inkluderades, och därmed uppstod behovet av att kunna konvertera mellan de två. 

Alternativen för konvertering till små bokstäver i PHP inkluderar även funktionerna `ucfirst` och `ucwords`. Dessa kommer att konvertera den första bokstaven i en sträng till stor bokstav respektive alla första bokstaver i varje ord till stor bokstav. Det är viktigt att notera att `strtolower` endast konverterar alla tecken i strängen och inte påverkar vare sig den första bokstaven eller andra bokstäver i ordet. 

Implementeringen av `strtolower` i PHP är mycket effektiv och använder sig av inbyggda funktioner för att konvertera varje tecken. Detta gör att processen går snabbt och utan större påverkan på prestandan.

## Se även

Officiell dokumentation för [strtolower()](https://www.php.net/manual/en/function.strtolower.php) i PHP.