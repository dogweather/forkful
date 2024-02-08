---
title:                "Analyser une date depuis une chaîne de caractères"
aliases:
- fr/powershell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:25.699625-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser une date à partir d'une chaîne de caractères consiste à reconnaître et à convertir des dates écrites sous forme de texte en un type de données date que PowerShell peut comprendre et traiter. Les programmeurs font cela pour manipuler, formater, comparer ou calculer des dates, des tâches courantes dans les scripts traitant des fichiers journaux, des entrées utilisateur ou du traitement de données.

## Comment faire :
PowerShell rend l'analyse des dates à partir de chaînes de caractères simple avec son applet de commande `Get-Date` et l'accélérateur de type `[datetime]`, qui fonctionnent bien pour les formats de date standard. Pour les chaînes de dates plus complexes ou non standard, la méthode `[datetime]::ParseExact` peut être utilisée pour spécifier le format exact.

### Utilisation de `Get-Date` et `[datetime]` :
```powershell
# Conversion simple en utilisant Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Exemple de sortie :**
```
Samedi 1 avril 2023 00:00:00
```

```powershell
# Utilisation de l'accélérateur de type [datetime]
$stringDate = "1 avril 2023"
$date = [datetime]$stringDate
echo $date
```
**Exemple de sortie :**
```
Samedi 1 avril 2023 00:00:00
```

### Utilisation de `[datetime]::ParseExact` pour des formats non standard :
Pour les formats non automatiquement reconnus, vous pouvez définir le format exact pour garantir un parsing correct.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Exemple de sortie :**
```
Samedi 1 avril 2023 14:00:00
```

### Tirer parti des bibliothèques tierces
Bien que PowerShell soit en soi assez puissant pour l'analyse des dates, pour des scénarios très complexes ou des fonctionnalités supplémentaires, vous pourriez explorer des bibliothèques .NET telles que NodaTime, bien que pour de nombreux cas d'utilisation typiques, les capacités natives de PowerShell suffiront.

```powershell
# Utilisation de NodaTime juste comme illustration, notez que vous devez ajouter la bibliothèque à votre projet
# Install-Package NodaTime -Version 3.0.5
# Utilisation de NodaTime pour analyser une date
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Note de l'exemple :** Le code ci-dessus est une illustration conceptuelle. En pratique, assurez-vous que NodaTime est correctement ajouté à votre projet pour que les types et méthodes soient disponibles.
