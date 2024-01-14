---
title:    "Fish Shell: Generera slumpmässiga nummer"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att kunna generera slumpmässiga nummer är en användbar färdighet i programmering. Det kan användas för att skapa unika ID-nummer, slumpmässiga lösenord eller för att simulera slumpmässiga händelser i spel eller tester. Det finns flera olika sätt att generera slumpmässiga nummer och i denna bloggpost kommer vi att fokusera på hur man gör det med Fish Shell.

## Hur man gör det

Att generera slumpmässiga nummer i Fish Shell är enkelt. Först måste vi definiera en variabel som kommer att hålla det slumpmässiga numret. Sedan kan vi använda inbyggda funktionen `rand` för att generera ett slumpmässigt nummer mellan 0 och 1. Om vi vill ha ett heltal istället för ett decimaltal, kan vi använda `ceil` för att avrunda uppåt.

```Fish Shell
set random_number (rand)
echo $random_number
# Output: 0.346784
set random_integer (ceil $random_number)
echo $random_integer
# Output: 1
```

Vi kan också ange ett intervall för det slumpmässiga numret med hjälp av `math` funktionen. Om vi vill generera ett nummer mellan 1 och 10, kan vi göra det så här:

```Fish Shell
set random_number (math "(rand * (10 - 1)) + 1")
echo $random_number
# Output: 4.375
set random_integer (ceil $random_number)
echo $random_integer
# Output: 5
```

Det finns flera andra sätt att generera slumpmässiga nummer och det beror på vad som behövs för din specifika användning. Med hjälp av dessa grundläggande exempel, kan du anpassa och experimentera för att hitta det bästa sättet för ditt projekt.

## Djupdykning

Om du vill ha lite mer kontroll över dina slumpmässiga nummer, kan du använda `math` funktionen och inbyggda variabler som `$status` och `$pid` för att skapa ett unikt nummer varje gång. Här är ett exempel på hur du kan göra det:

```Fish Shell
set seed (math "(status + pid)")
set random_number (math "(rand * 10) * $seed")
echo $random_number
# Output: 6.642
set random_integer (ceil $random_number)
echo $random_integer
# Output: 7
```

Som du kan se är det möjligt att skapa mer komplexa slumpmässiga nummer med hjälp av olika variabler och funktioner som finns tillgängliga i Fish Shell. Det finns också flera andra metoder som du kan använda för att generera slumpmässiga nummer som du kan utforska och lära dig mer om.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/)

- [How to Generate Random Numbers in Bash](https://linuxhint.com/generate_random_number_bash/)

- [Generating Random Numbers in Python](https://realpython.com/python-random/)