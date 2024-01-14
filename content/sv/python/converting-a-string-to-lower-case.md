---
title:    "Python: Konvertera en sträng till gemener"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Varför
Att konvertera en sträng till små bokstäver är en vanlig operation inom programmering och kan vara användbart av flera skäl. Det kan hjälpa till att enhetliggöra indata, göra sökningar och jämförelser mer exakta och ge en mer estetiskt tilltalande output.

## Hur man gör
För att konvertera en sträng till små bokstäver i Python används funktionen `lower()`. Detta kan göras på en sträng som tilldelats till en variabel eller direkt på en sträng som skrivs in som indata.

```Python
# Exempel på att använda lower() på en variabel
namn = "SARA"
lågernamn = namn.lower()
print(lågernamn)

# Exempel på att använda lower() direkt på inmatat värde
print(input("Skriv ett ord: ").lower())
```

Output:

```
sara
Skriv ett ord: PYTHON
python
```

## Djupdykning
När en sträng konverteras till små bokstäver, används det inbyggda metoden `lower()` som finns tillgänglig på varje sträng-objekt i Python. Till skillnad från att använda `upper()` som konverterar alla bokstäver till stora, förändras bara bokstäverna som redan är små i `lower()`. Detta innebär att om strängen innehåller andra tecken såsom siffror eller specialtecken, förblir dessa oförändrade. Det är också viktigt att notera att `lower()` inte ändrar den ursprungliga variabeln utan returnerar en ny sträng med konverterade bokstäver.

### Undvik felmeddelanden
En viktig aspekt att tänka på när man använder `lower()` är att se till att den sträng som arbetas med är av typen `str`. Om en variabel är av en annan typ, som t.ex. `int` eller `float`, kommer det att uppstå ett felmeddelande. Det är då viktigt att först konvertera variabeln till typen `str` för att undvika detta.

### Användbarhet i strängmanipulering
Att kunna konvertera en sträng till små bokstäver är också användbart när man gör jämförelser eller sökningar i en applikation. Genom att konvertera både inmatade värden och data i en databas till små bokstäver kan man säkerställa att matchningar blir korrekta oavsett om bokstäverna är små eller stora. Detta blir särskilt användbart när man jobbar med användarinput och vill undvika fall där felaktiga matchningar kan göra att applikationen inte fungerar som förväntat.

## Se även
- [Python dokumentation om `lower()`](https://docs.python.org/sv/3/library/stdtypes.html#str.lower)
- [Python tutorial om strängar](https://docs.python.org/sv/3/tutorial/introduction.html#strings)
- [Blogginlägg om varför man ska konvertera strängar till små bokstäver](https://stackabuse.com/how-to-convert-strings-to-lowercase-and-uppercase-in-python/)