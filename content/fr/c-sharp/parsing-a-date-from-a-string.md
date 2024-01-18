---
title:                "Analyser une date à partir d'une chaîne de caractères"
html_title:           "C#: Analyser une date à partir d'une chaîne de caractères"
simple_title:         "Analyser une date à partir d'une chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Parsing une date à partir d'une chaîne de caractères est le processus de conversion d'une date écrite sous forme de texte en une valeur de date reconnue par l'ordinateur. Les programmeurs le font pour pouvoir manipuler et utiliser des données de date dans leurs applications.

## Comment faire:

Voici deux exemples de code en C# montrant comment parser une date à partir d'une chaîne de caractères:

```C#
// Exemple 1:
string dateStr = "01/01/2021";
DateTime date = DateTime.ParseExact(dateStr, "dd/MM/yyyy", CultureInfo.InvariantCulture);
Console.WriteLine(date); // Output: 01/01/2021 00:00:00

// Exemple 2:
string dateStr = "2021-02-15";
DateTime date = DateTime.Parse(dateStr);
Console.WriteLine(date); // Output: 15/02/2021 00:00:00
```

## Plongée en profondeur:

Le parsing de dates à partir de chaînes de caractères est un concept important en informatique, en particulier dans le développement d'applications web. Il existe plusieurs alternatives au parsing de dates, notamment en utilisant des bibliothèques telles que NodaTime ou en utilisant des formats de dates standardisés comme ISO 8601. L'implémentation exacte peut varier selon le langage de programmation utilisé, mais le processus reste le même: convertir une date écrite en texte en une valeur de date utilisable.

## Voir aussi:

Pour en savoir plus sur le parsing de dates en C#, vous pouvez consulter les ressources suivantes:

- [Documentation officielle de Microsoft sur la classe DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0)
- [Tutoriel sur le parsing de dates en C#](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [NodaTime, une alternative à la classe DateTime de C#](https://nodatime.org/)
- [Norme ISO 8601 pour les dates](https://www.iso.org/iso-8601-date-and-time-format.html)