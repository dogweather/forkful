---
title:    "Fish Shell: Att hitta längden på en sträng"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en grundläggande färdighet inom Fish Shell-programmering. Oavsett om du behöver kontrollera en inmatning eller manipulera strängar i ett skript, är det viktigt att veta hur man enkelt kan beräkna längden på en sträng. I den här bloggposten kommer vi att utforska olika metoder för att hitta längden på en sträng i Fish Shell.

## Hur man gör

För att hitta längden på en sträng i Fish Shell kan du använda inbyggda kommandon och funktioner eller skapa dina egna kodsnuttar. Låt oss börja med att använda inbyggda kommandon.

```Fish Shell
# Skapa en variabel med en sträng
set sträng 'Välkommen till Fish Shell'

# Använd kommandot `string length` för att hitta längden på strängen
string length $sträng

# Resultat: 25
```

Som du kan se är kommandot `string length` enkelt att använda och returnerar längden på en sträng som ett heltal. Men om du vill ha mer kontroll över hur längden beräknas, kan du skapa din egen funktion.

```Fish Shell
# Definiera en funktion som hittar längden på en sträng
function hitta_längd
  set sträng (echo $argv | tr '' \n) # Använd <en trappa> för att omvandla varje tecken i strängen till en ny rad
  set längd (count $sträng) # Använd funktionen `count` för att räkna antalet rader i strängen
  echo $längd # Skriv ut längden
end

# Anropa funktionen med en sträng som argument
hitta_längd 'Hej!'

# Resultat: 4
```

Som du ser kan du anpassa din lösning och använda olika inbyggda funktioner för att kontrollera längden på en sträng.

## Djupare dykning

Om du vill ha mer djupgående kunskap om hur du kan hitta längden på en sträng i Fish Shell, kan du utforska andra inbyggda kommandon och funktioner som `string match` eller `string sub`. Du kan också lära dig mer om hur du kan manipulera strängar med hjälp av variabler och loopar.

Se till att också utforska andra användbara verktyg som Fish Shell erbjuder, som till exempel substitutionskommandon, vilket kanske kan vara mer effektiva för vissa användningsfall.

## Se också

- Fish Shell dokumentation: https://fishshell.com/docs/current/index.html
- Exempelskript för Fish Shell: https://github.com/fish-shell/fish-shell/blob/master/doc_src/tutorials/examples.md
- Meddelande "Unofficial Fish Shell User Guide": https://github.com/oh-my-fish/oh-my-fish/blob/master/docs/Unofficial-Fish-Shell-User-Guide.md