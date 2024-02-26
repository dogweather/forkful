---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:55.734342-07:00
description: "Het combineren van strings, of concatenatie, is zoals het maken van\
  \ een trein van woorden. We doen het om teksten samen te voegen, zinnen te vormen,\
  \ of\u2026"
lastmod: '2024-02-25T18:49:48.354534-07:00'
model: gpt-4-0125-preview
summary: "Het combineren van strings, of concatenatie, is zoals het maken van een\
  \ trein van woorden. We doen het om teksten samen te voegen, zinnen te vormen, of\u2026"
title: Samenvoegen van strings
---

{{< edit_this_page >}}

## Wat & Waarom?
Het combineren van strings, of concatenatie, is zoals het maken van een trein van woorden. We doen het om teksten samen te voegen, zinnen te vormen, of eender welke situatie waarin afzonderlijke strings maatjes moeten worden en één geheel vormen.

## Hoe:
Laten we er meteen induiken:

```PowerShell
# Met de '+' operator
$greeting = 'Hallo, ' + 'Wereld!'
$greeting # Geeft uit: Hallo, Wereld!

# Via string interpolatie
$name = 'Jane'
$welcomeMessage = "Hoi, $name, leuk je te ontmoeten!"
$welcomeMessage # Geeft uit: Hoi, Jane, leuk je te ontmoeten!

# Met de -f operator (formaatoperator)
$city = 'New York'
$visitMessage = 'Welkom in {0}!' -f $city
$visitMessage # Geeft uit: Welkom in New York!

# StringBuilder voor complexe scenario's (een beetje overkill voor simpele dingen)
$textBuilder = New-Object System.Text.StringBuilder
[void]$textBuilder.Append('PowerShell ')
[void]$textBuilder.Append('is ')
[void]$textBuilder.Append('geweldig.')
$textBuilder.ToString() # Geeft uit: PowerShell is geweldig.
```

## Diepgaand
Historisch gezien was het concatenatie van strings een beetje ruw aan de randen in eerdere programmeertalen - denk eraan alsof je zinnen aan elkaar plakt met tape. In PowerShell is het een fluitje van een cent.

Er zijn verschillende manieren om het werk gedaan te krijgen. De '+' operator is eenvoudig maar kan traag zijn met veel strings. String interpolatie met "$variable" is schoner en geweldig voor het invoegen van variabelen in strings. De formaatoperator '-f' schittert in templating-scenario's.

Over prestaties - als je een essay aan strings samen voegt, wil je iets robuuster. Voer `StringBuilder` in. Het voegt niet onmiddellijk samen; in plaats daarvan weeft het je strings samen wanneer opgeroepen, tijd en geheugen besparend voor grote concatenatietaken.

## Zie ook
- [Over Join](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.3)
- [Over Automatische Variabelen](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.3) (zie `$OFS`)
- Voor meer over stringformatting, bekijk [Samengesteld Formatteren](https://docs.microsoft.com/nl-nl/dotnet/standard/base-types/composite-formatting).
- En, als je er de maag voor hebt, hier is het fijne van de zaak over [StringBuilder](https://docs.microsoft.com/nl-nl/dotnet/api/system.text.stringbuilder?view=net-6.0).
