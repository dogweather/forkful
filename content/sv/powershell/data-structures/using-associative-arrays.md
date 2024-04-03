---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:40.208846-07:00
description: "Hur man g\xF6r: Att skapa och anv\xE4nda associativa arrayer i PowerShell\
  \ \xE4r ganska enkelt. S\xE5 h\xE4r g\xF6r du magin: **Att skapa en associativ array:**."
lastmod: '2024-03-13T22:44:38.117135-06:00'
model: gpt-4-0125-preview
summary: "Att skapa och anv\xE4nda associativa arrayer i PowerShell \xE4r ganska enkelt."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
Att skapa och använda associativa arrayer i PowerShell är ganska enkelt. Så här gör du magin:

**Att skapa en associativ array:**

```PowerShell
$minAssociativArray = @{}
$minAssociativArray["namn"] = "Alex"
$minAssociativArray["ålder"] = 25
$minAssociativArray["jobb"] = "Ingenjör"
```

Denna kodsnutt skapar en associativ array med tre nyckel-värdepar.

**Att komma åt värden:**

För att få ett värde, referera till dess nyckel:

```PowerShell
Write-Output $minAssociativArray["namn"]
```

**Exempel på utdata:**

```
Alex
```

**Att lägga till eller modifiera data:**

Använd bara nyckeln för att lägga till ett nytt par eller modifiera ett befintligt:

```PowerShell
$minAssociativArray["plats"] = "New York" # Lägger till ett nytt nyckel-värdepar
$minAssociativArray["jobb"] = "Senior Ingenjör" # Modifierar ett befintligt par
```

**Att iterera över en associativ array:**

Loopa igenom nycklar och värden på detta sätt:

```PowerShell
foreach ($nyckel in $minAssociativArray.Keys) {
  $värde = $minAssociativArray[$nyckel]
  Write-Output "$nyckel : $värde"
}
```

**Exempel på utdata:**

```
namn : Alex
ålder : 25
jobb : Senior Ingenjör
plats : New York
```

## Att fördjupa sig
Konceptet med associativa arrayer är vanligt över många programmeringsspråk, vanligtvis kallat en ordbok, kartläggning eller hashtabell beroende på språket. I PowerShell implementeras associativa arrayer som hashtabeller, vilket är ganska effektivt för att söka upp nycklar, lagra data och underhålla en samling av unika nycklar.

Historiskt sett ger associativa arrayer ett sätt att hantera samlingar av objekt där varje artikel kan hämtas snabbt utan att iterera genom hela samlingen, med hjälp av dess nyckel. Effektiviteten av dataåtervinning och modifiering i associativa arrayer gör dem till ett föredraget val för olika uppgifter. De har dock begränsningar, som att bibehålla ordning, för vilka ordnade ordböcker eller anpassade objekt kan vara ett bättre alternativ.

Trots deras begränsningar är associativa arrayer/hashtabeller i PowerShell otroligt flexibla och ett kraftfullt verktyg för skriptning. De tillåter dynamisk datalagring och är särskilt användbara i konfigurationer, datamanipulation och överallt där ett strukturerat dataformat behövs utan bördan av en formell klassdefinition. Kom bara ihåg, medan associativa arrayer är perfekta för nyckelbaserad återhämtning, om din uppgift innebär komplexa datastrukturer eller kräver att en specifik ordning bibehålls, kanske du vill utforska andra datatyper eller anpassade objekt inom PowerShell.
