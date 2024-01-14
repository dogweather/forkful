---
title:                "Python: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett visst mönster är en användbar programmeringsfunktion när man behöver hantera strängar och manipulera dem på olika sätt. Det kan hjälpa till att rensa data, formatera text eller filtrera bort oönskade tecken.

## Så här gör du

För att ta bort tecken som matchar ett visst mönster i Python använder man metoden "sub" från "re" biblioteket. Denna metod gör det möjligt att söka efter ett visst mönster och ersätta det med en annan sträng.

```Python
import re

# Skapa en sträng
string = "Hej världen!"
print(string)

# Använd metoden sub för att ta bort alla mellanslag i strängen
print(re.sub("\s", "", string))

# Output: Hejvärlden!
```

Metoden "sub" tar två argument, det första är det sökta mönstret och det andra är ersättningssträngen. I vårt exempel använde vi "\s" som sökmönster vilket letar efter alla mellanslag i strängen och ersätter dem med en tom sträng.

Vi kan också använda "sub" metoden för att ta bort andra tecken som inte matchar ett visst mönster. Till exempel om vi bara vill behålla bokstäver och ta bort siffror och specialtecken från en sträng.

```Python
import re

# Skapa en sträng med bokstäver, siffror och specialtecken
string = "Hej, mitt telefonnummer är 123-456-789!"
print(string)

# Ta bort alla tecken som inte är bokstäver
print(re.sub("[^a-zA-Z]", "", string))

# Output: Hejmitttelefonnummerär
```

I detta exempel använde vi mönstret "[^a-zA-Z]" vilket betyder att vi vill behålla alla bokstäver från a till z och A till Z och ta bort alla andra tecken.

## Djupdykning

Python erbjuder många olika möjligheter att manipulera strängar och metoden "sub" är bara en av dem. Om man vill ha mer kontroll över vilka tecken som tas bort eller behålls kan man använda sig av reguljära uttryck, med hjälp av "re" biblioteket. Detta låter användaren specificera mer komplexa mönster för att matcha och manipulera strängar.

%%Markdown
## Se även

- Python dokumentation för "re" biblioteket: https://docs.python.org/3/library/re.html
- Reguljära uttryck tutorial för Python: https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial