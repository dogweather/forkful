---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:51.974936-07:00
description: "Att g\xF6ra det f\xF6rsta tecknet i en str\xE4ng till versal i Bash\
  \ involverar att omvandla det f\xF6rsta tecknet av str\xE4ngen till stort bokstav\
  \ medan resten av\u2026"
lastmod: 2024-02-19 22:04:57.292310
model: gpt-4-0125-preview
summary: "Att g\xF6ra det f\xF6rsta tecknet i en str\xE4ng till versal i Bash involverar\
  \ att omvandla det f\xF6rsta tecknet av str\xE4ngen till stort bokstav medan resten\
  \ av\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
---

{{< edit_this_page >}}

## Vad & Varför?
Att göra det första tecknet i en sträng till versal i Bash involverar att omvandla det första tecknet av strängen till stort bokstav medan resten av strängen lämnas oförändrad. Denna teknik används ofta för att formatera utdata eller för att följa kodkonventioner som kräver att vissa strängar börjar med en stor bokstav för läsbarhet eller stilistiska preferenser.

## Hur man gör:

Bash har inte en inbyggd funktion specifikt för att göra bokstäver till versaler i strängar, men du kan åstadkomma detta med hjälp av parameterexpansion eller externa verktyg som `awk`. Här är några sätt att skriva det första tecknet i en sträng med stor bokstav i Bash:

**Använda Parameter Expansion:**

Denna metod manipulerar strängen direkt i skalet.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
Utdata:
```
Hello world
```

**Använda `awk`:**

`awk` är ett kraftfullt verktyg för textbearbetning som är tillgängligt på de flesta Unix-liknande operativsystem, vilket kan användas för att skriva strängar med stor bokstav.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Utdata:
```
Hello world
```

**Använda `sed`:**

För en mer traditionell ansats kan `sed` användas för att göra det första tecknet i en sträng till stor bokstav. Dock är det lite mer komplext jämfört med de tidigare metoderna.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
Utdata:
```
Hello world
```

Dessa kodsnuttar demonstrerar hur man gör det första tecknet i en sträng till versal i Bash, vilket belyser flexibiliteten i shellskriptning när det gäller textmanipulering.
