---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hålla det Kort och enkelt med Substrängar i Java

## Varför och Varför?

Extrahering av substrängar involverar att hämta en specifik del av en sträng. Programmers använder det för att interagera med och manipulera data effektivt inom strängen.

## Hur man gör:

Utvunna substrängar i Java är enkla. De två vanligaste metoderna vi använder är `substring(int beginIndex)` och `substring(int beginIndex, int endIndex)`. Här är ett exempel:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hej Världen!";
        System.out.println(str.substring(4)); //utskrift: Världen!
        System.out.println(str.substring(0, 3)); //utskrift: Hej
    }
}
```
I det första fallet börjar understrängen vid den angivna indexeringen och strecker sig till slutet av strängen. I det andra fallet anger du både start och slut.

## Fördjupning

**Historisk sammanhang:** 
Metoderna substring i Java har varit en del av Java Standard Edition sedan version 1.0, och de är en del av Java.lang.String-klassen.

**Alternativ:**
Vissa situationer kan kräva användning av andra strängfunktioner som `split()`, `indexOf()` eller biblioteket `Apache Commons Lang`, specifikt `StringUtils.substring()` som är mer robust.

**Implementering detaljer:**
Notera att strängindexering börjar från noll. Djupare ser vi att substrängsmetoderna skapar en ny strängobjekt, allmänt resulterar detta i ökad minnesanvändning.

## Se Även

1. Java String substring() metod: [Officiell Java-dokumentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int))
2. Java String API-översikt: [Oracle Java-dokumentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
3. Apache Commons Lang: [Officiell Apache dokumentation](http://commons.apache.org/proper/commons-lang/)

Skapa kraftfulla program med substrängar, och lycka till med din kodning!