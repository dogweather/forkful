---
title:    "C#: Ekstrahering av substringer"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor vil noen ønske å trekke ut substringer fra en tekststreng? Det kan være flere årsaker til dette, men et vanlig tilfelle er å manipulere data eller søke etter spesifikke mønstre i en tekststreng. Dette kan være nyttig i programmering og databehandling, spesielt når man arbeider med store datamengder.

# Slik gjør du det

For å trekke ut substringer i C#, trenger vi å bruke metoden "Substring". Denne metoden tar to argumenter - startindeks og lengde. Startindeks angir i hvilket tegn vi skal starte å trekke ut substringen, mens lengden angir hvor mange tegn vi vil trekke ut. La oss se på et eksempel på hvordan vi kan bruke denne metoden:

```C#
string tekst = "Dette er en tekststreng";
string utdrag = tekst.Substring(8, 2);
Console.WriteLine(utdrag);
// Output: en
```

I dette eksempelet trekker vi ut de to tegnene som starter på indeks 8, som i dette tilfellet er "e" og "n". Vi kan også trekke ut substringen fra en bestemt indeks til slutten av strengen ved å bare oppgi ett argument, som i dette tilfellet:

```C#
string utdrag = tekst.Substring(13);
Console.WriteLine(utdrag);
// Output: tekststreng
```

# Dypdykk

I eksemplene våre brukte vi bare en startindeks og lengde for å trekke ut substringen. Men i virkeligheten kan vi også bruke negative tall som startindeks, noe som betyr at vi teller baklengs fra slutten av teksten. Vi kan også kombinere dette med den andre versjonen av "Substring" metoden hvor vi bare oppgir startindeks, og la den trekke ut resten av teksten. La oss se på et eksempel på dette:

```C#
string utdrag = tekst.Substring(-8);
Console.WriteLine(utdrag);
// Output: tekststreng
```

Her teller vi åtte tegn baklengs fra enden av teksten, som betyr at vi starter på "t" og trekker ut resten av teksten. Dette kan være nyttig i tilfeller der vi ikke vet nøyaktig lengden på substringen vi trenger å trekke ut.

# Se også

- [Microsoft sin dokumentasjon om "Substring" metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [En tutorial om hvordan man bruker "Substring" i C#](https://www.tutlane.com/tutorial/csharp/csharp-substring)