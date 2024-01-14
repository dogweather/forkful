---
title:    "PHP: Store bokstaver i en streng"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ønske å kapitalisere en streng i PHP. Kanskje du vil formatere tekst før du skriver den ut, eller kanskje du vil gjøre navn eller titler mer lesbare. Uansett hva årsaken er, vil denne funksjonen bidra til å gjøre koden din mer effektiv og profesjonell.

## Hvordan

For å kapitalisere en streng i PHP, kan du bruke funksjonen `strtoupper()`. Dette vil konvertere alle bokstavene i strengen til store bokstaver. For eksempel:

```PHP
$str = "hei verden";
echo strtoupper($str);
```

Dette vil gi ut følgende resultat:

```PHP
HEI VERDEN
```

Hvis du kun ønsker å kapitalisere den første bokstaven i strengen, kan du bruke funksjonen `ucfirst()`. Dette vil bare endre den første bokstaven til stor, mens resten av strengen forblir uendret. For eksempel:

```PHP
$str = "hallo";
echo ucfirst($str);
```

Dette vil gi ut følgende resultat:

```PHP
Hallo
```

## Dypdykk

Hvis du ønsker å kapitalisere en streng som inneholder flere ord, kan du bruke funksjonen `ucwords()`. Denne vil konvertere den første bokstaven i hvert ord til stor, mens resten av strengen forblir uendret. For eksempel:

```PHP
$str = "hei verden";
echo ucwords($str);
```

Dette vil gi ut følgende resultat:

```PHP
Hei Verden
```

Det er også muligheter for å kapitalisere spesifikke deler av en streng ved å bruke funksjoner som `strrpos()` og `substr()`, men dette kan være litt mer avansert og er utenfor omfanget av denne bloggposten.

## Se Også

- `strtoupper()` - PHP Manual (https://www.php.net/manual/en/function.strtoupper.php)
- `ucfirst()` - PHP Manual (https://www.php.net/manual/en/function.ucfirst.php)
- `ucwords()` - PHP Manual (https://www.php.net/manual/en/function.ucwords.php)