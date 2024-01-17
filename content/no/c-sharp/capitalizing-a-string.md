---
title:                "Gjøre strengen stor"
html_title:           "C#: Gjøre strengen stor"
simple_title:         "Gjøre strengen stor"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kapitalisering av en streng betyr å gjøre den første bokstaven i hvert ord til en stor bokstav. Dette gjøres av programmere for å gjøre teksten mer lesbar og for å følge vanlig konvensjon for tekstutforming.

## Hvordan:

```
void Main() 
{ 
    string inputString = "hei, dette er en tekst som skal kapitaliseres."; 
    Console.WriteLine(capitalizeString(inputString)); 
} 

string capitalizeString(string str) 
{ 
    string[] words = str.Split(' '); 
    for (int i = 0; i < words.Length; i++) 
    { 
        words[i] = words[i].Substring(0, 1).ToUpper() + words[i].Substring(1); 
    } 
    return String.Join(" ", words); 
} 

// Output: Hei, Dette Er En Tekst Som Skal Kapitaliseres.
```

## Dypdykk:

Historisk sett har kapitalisering blitt brukt i trykte tekster for å markere begynnelsen av en setning eller et navn. I dag brukes det i programmering for å følge konsistente konvensjoner og for å lette lesbarheten av koden.

Alternativt kan man også bruke funksjonen `ToTitleCase()` i C# for å enkelt kapitalisere en streng. Dette fungerer imidlertid ikke alltid som forventet for språk som ikke følger det engelske alfabetet.

Implementeringen som er vist i koden over tar hensyn til at en streng kan inneholde flere ord og sørger for å kapitalisere hver eneste et. Den benytter seg også av string operations og en for-løkke for å iterere gjennom hvert ord i strengen.

## Se også:

- [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.totitlecase?view=net-5.0) for mer informasjon om `ToTitleCase()`-funksjonen i C#.
- [Wikipedia](https://no.wikipedia.org/wiki/Stor_og_liten_bokstav) for en oversikt over bruken av kapitalisering i språk generelt.