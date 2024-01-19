---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Convertir une date en chaîne (string) est le processus de transformation de l'objet DateTime (qui représente des dates et des temps) en format de chaîne de caractères. Les programmeurs le font pour faciliter la présentation lisible de la date à l'utilisateur ou pour la sauvegarder dans une base de données qui accepte uniquement des formats string.

## Comment faire:

Voici quelques exemples de code C# pour illustrer comment vous pouvez convertir un objet DateTime en chaîne.

```C#
DateTime dt = DateTime.Now;
Console.WriteLine(dt.ToString());
```
L'exemple ci-dessus utilise simplement la méthode ToString() pour afficher la date et l'heure actuelles.

Sortie attendue:

```
12/09/2022 10:00:50 AM
```

Vous pouvez également spécifier le format de sortie que vous voulez.

```C#
DateTime dt = DateTime.Now;
Console.WriteLine(dt.ToString("MM-dd-yyyy"));
```

Ici, la date est présentée au format mois-jour-année.

Sortie attendue:

```
12-09-2022
```

## Plongée en profondeur:

Lorsque .Net Framework a été introduit pour la première fois, il a inclus un concept appelé "formatting and parsing," qui concerne la conversion entre types de données de base et strings. La conversion de DateTime en string en fait partie.

Il existe des alternatives à la méthode ToString(). Par exemple, la méthode String.Format() peut être utilisée pour formater et convertir un DateTime. 

```C#
DateTime dt = DateTime.Now;
Console.WriteLine(String.Format("{0:MM/dd/yyyy}", dt));
```

En ce qui concerne les détails de mise en œuvre, la conversion d'un DateTime en string est faite en interne en utilisant la méthode native FormatHelper qui gère les spécificités du formatage.

## Voir également:

1. [Documentation Microsoft sur la classe DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime)
2. [Documentation Microsoft sur la méthode ToString()](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.tostring)
3. [Explication détaillée du format string pour les dates](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/standard-date-and-time-format-strings)