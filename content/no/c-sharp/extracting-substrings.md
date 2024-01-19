---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å splitte opp strenger er teknikken ved å hente ut mindre deler ("substrings") fra en lengre tekst ("string"), basert på gitt indeksposisjon. Programmerere gjør dette for å analysere eller bearbeide data effektivt og på en måte som gir mening i gitte kontekster.

## Hvordan: 

Koden nedenfor viser hvordan man uthenter en substring i C#.

```csharp
string tekst = "Hei, Verden!";
string delTekst = tekst.Substring(4, 6);

Console.WriteLine(delTekst);
```

Output:

```csharp
", Verd"
```

Her har vi tatt ut teksten som starter på den 4. posisjonen, og går 6 tegn fremover i teksten. Merk at vi teller posisjonene fra 0.

## Dyp Dykk:

Substring-metoden har vært tilstede i mange varianter av programmeringsspråk gjennom alle år, og er en standard virkemåte for å manipulere tekststrenger. Det fins flere alternativer, spesielt i C#, som `Remove()`, `IndexOf()` og `Split()`, og hvilken en bør velge kommer an på den spesifikke oppgaven.

Implementasjonen av `Substring()`-metoden i C# er rask og effektiv, men man bør være oppmerksom på potensielle unntak som kan oppstå dersom startindeksen er utenfor tegnrekkevidden, eller hvis antall tegn som skal hentes ut overstiger den gjenværende lengden på teksten fra startindeksen.

## Se også:

1. MSDN String.Substring Method: [Link](https://msdn.microsoft.com/library/aka44szs.aspx)
2. C# Corner: Understanding String Manipulation in C#: [Link](https://www.c-sharpcorner.com/article/understanding-string-manipulation-in-c-sharp/)
3. Dot Net Perls - Substring: [Link](https://www.dotnetperls.com/substring)