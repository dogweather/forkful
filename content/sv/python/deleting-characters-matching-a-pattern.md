---
title:    "Python: Raderar tecken som matchar ett mönster"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att ta bort karaktärer som matchar ett mönster är en vanlig uppgift inom Python-programmering. Det kan vara till nytta för att rensa och organisera data eller för att filtrera ut oönskade tecken. Genom att lära sig hur man gör detta kan du enkelt hantera textsträngar på ett effektivt sätt.

## Hur man gör det

För att ta bort karaktärer som matchar ett visst mönster i en textsträng kan du använda funktionen `re.sub()` från standardmodulen `re`. Med hjälp av reguljära uttryck kan man definiera vilka tecken som ska tas bort. Här är ett exempel på hur du kan ta bort alla siffror från en textsträng:

```python
import re
text = "Det finns 91 elever på skolan"
rensad_text = re.sub(r"\d+", "", text)
print(rensad_text)
```

Output:
`Det finns elever på skolan`

För att förstå hur koden fungerar kan vi bryta ner den lite. Funktionen `re.sub()` tar tre argument: ett reguljärt uttryck, ersättningstexten och textsträngen som ska bearbetas.

- Reguljära uttryck är strängar som definierar ett mönster som vi vill matcha i texten. I detta fall används `\d+` som matchar en eller flera siffror.
- Ersättningstexten är den nya texten som kommer att ersätta det matchade mönstret, i detta fall är det en tom sträng (`""`).
- Slutligen är textsträngen som ska bearbetas den variabel som vi har definierat som `text`.

Genom att köra `re.sub()` på textsträngen `text` får vi resultatet som tilldelas variabeln `rensad_text`. Denna textsträng kan sedan användas för olika ändamål.

## Djupdykning

Att förstå och använda reguljära uttryck är en viktig del inom Python-programmering. I exemplet ovan använde vi uttrycket `\d+` för att matcha alla siffror. Här är några fler användbara uttryck för att ta bort karaktärer från en textsträng:

- `\w`: Matchar alla alfanumeriska tecken (a-z, A-Z, 0-9) och understreck.
- `.`: Matchar alla tecken utom radslutningstecken.
- `[^a-z]`: Matchar alla tecken som inte är små bokstäver från a till z.

Det finns många fler alternativ att utforska och det kan vara till hjälp att använda en online-reguljär-expressionseditor för att testa och experimentera med olika mönster.

## Se även

- [Dokumentation för re.sub()](https://docs.python.org/3/library/re.html#re.sub)
- [Regexpal - Online reguljär-expressionseditor](https://regexpal.com/)