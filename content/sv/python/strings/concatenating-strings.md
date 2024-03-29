---
date: 2024-01-20 17:35:41.330369-07:00
description: "Sammanfogning av str\xE4ngar, eller \"concatenation\" som det ofta kallas,\
  \ inneb\xE4r att du kopplar ihop tv\xE5 eller flera textstycken till en enda str\xE4\
  ng.\u2026"
lastmod: '2024-03-13T22:44:37.475297-06:00'
model: gpt-4-1106-preview
summary: "Sammanfogning av str\xE4ngar, eller \"concatenation\" som det ofta kallas,\
  \ inneb\xE4r att du kopplar ihop tv\xE5 eller flera textstycken till en enda str\xE4\
  ng.\u2026"
title: "Sammanslagning av str\xE4ngar"
---

{{< edit_this_page >}}

## Vad & Varför?

Sammanfogning av strängar, eller "concatenation" som det ofta kallas, innebär att du kopplar ihop två eller flera textstycken till en enda sträng. Programmerare gör detta för att skapa meningar, meddelanden eller för bearbetning av text.

## Hur man gör:

För att sammanfoga strängar i Python används plus-tecknet (+) eller `join()`-metoden. Kika på exemplen nedan:

```python
# Använda plus-tecknet
halsning = "Hej, " + "världen!"
print(halsning)  # Output: Hej, världen!

# Använda join()-metoden
namn = ["Stockholm", "Göteborg", "Malmö"]
stad_str = ", ".join(namn)
print("Städerna: " + stad_str)  # Output: Städerna: Stockholm, Göteborg, Malmö
```

## Djupdykning:

Att sammanfoga strängar är grundläggande i många programmeringsspråk och var även en del av tidiga versionsutgåvor av Python. Alternativ till `+` och `join()` inkluderar formatteringsmetoder såsom %-formattering, `str.format()`, och f-stängar (från Python 3.6+).

- %-formattering: `halsning = "Hej, %s" % "världen!"`
- `str.format()`: `halsning = "Hej, {}".format("världen!")`
- f-strängar: `halsning = f"Hej, {'världen!'"}`

Varje metod har sina användningsområden. Till exempel är `join()` ideal för sammanfogning av en lista med strängar, medan f-strängar är utmärkta för inbäddning av variabler och uttryck direkt i strängen.

När det gäller prestanda, är f-strängar och `join()` vanligtvis snabbare än att använda `+`, särskilt för stora och många strängar. Detta beror på att `+` skapar nya strängobjekt för varje sammanfogning vilket kan vara resurskrävande.

## Se även:

- Python officiell dokumentation om strängmetoder: https://docs.python.org/3/library/stdtypes.html#string-methods
- Python officiell dokumentation om formatsträngar: https://docs.python.org/3/library/string.html#formatstrings
- En djupdykning i Python-strängar och deras behandling: https://realpython.com/python-strings/
