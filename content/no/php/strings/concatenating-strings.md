---
date: 2024-01-20 17:35:31.354207-07:00
description: "How to (Slik gj\xF8r du det) Bruk punktum `.` for \xE5 koble sammen\
  \ strenger i PHP. Her er et enkelt eksempel."
lastmod: '2024-04-05T22:37:49.240714-06:00'
model: gpt-4-1106-preview
summary: "How to (Slik gj\xF8r du det) Bruk punktum `.` for \xE5 koble sammen strenger\
  \ i PHP. Her er et enkelt eksempel."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## How to (Slik gjør du det)
Bruk punktum `.` for å koble sammen strenger i PHP. Her er et enkelt eksempel:

```PHP
$hello = "Hallå";
$world = "Verden";
$message = $hello . " " . $world . "!";
echo $message;
```

Output:
```
Hallå Verden!
```

Du kan også bruke konkatenasjon for å bygge opp en streng:

```PHP
$name = "Ola";
$greeting = "Hei, " . $name . ", hvordan går det?";
echo $greeting;
```

Output:
```
Hei, Ola, hvordan går det?
```

Eller for å sette sammen flere variabler og tekst:

```PHP
$firstPart = "Følg";
$secondPart = "meg til";
$thirdPart = "Oslo!";
$route = $firstPart . " " . $secondPart . " " . $thirdPart;
echo $route;
```

Output:
```
Følg meg til Oslo!
```

## Deep Dive (Dypdykk)
Konkatenasjon i PHP har sine røtter i tidlige programmeringsspråk hvor operasjoner på tekst var helt grunnleggende. PHP har i seg mekanismer fra C, Perl og andre språk, hvor konkatenasjonsoperatoren (.) ble grunnleggende.

En alternativ måte å sette sammen strenger på er gjennom interpolering, som er enklere når man arbeider med mange variabler:

```PHP
$name = "Nora";
$place = "Lillehammer";
$time = "18:00";
$invitation = "Hei $name, treffes vi i $place kl $time?";
echo $invitation;
```

Implementasjonsdetaljer:
Konkatenasjon i PHP skjer ved at PHP bruker intern buffer for å sette sammen strengene før de vises eller behandles videre, noe som kan ha effekt på ytelsen ved store mengder data. Vurder derfor å bruke operatoren `.` sparsomt eller alternative metoder som `implode()` eller heredoc/nowdoc syntaks for større tekstblokker.

## See Also (Se også)
- PHP offisiell dokumentasjon om strengoperasjoner: https://www.php.net/manual/en/language.operators.string.php
- En tutorial om strenginterpolering: https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing
- Diskusjoner på Stack Overflow om beste praksis for konkatenasjon: https://stackoverflow.com/search?q=php+string+concatenation
