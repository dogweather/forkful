---
title:    "C#: Convertir une date en chaîne de caractères."
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Pourquoi : Il est important de savoir comment convertir une date en chaîne de caractères en programmation C#, surtout lorsqu'on travaille avec des données de nature temporelle.

Comment Faire :

```C#
using System;

public class DateConversion
{
    public static void Main()
    {
        // Déclarer une variable de type DateTime avec une valeur de date
        DateTime maDate = new DateTime(2021, 07, 15);

        // Utiliser la méthode ToString() pour convertir la date en chaîne de caractères
        string maDateEnChaine = maDate.ToString();

        // Afficher le résultat
        Console.WriteLine("La date en chaîne de caractères : " + maDateEnChaine);
        // La sortie sera : "La date en chaîne de caractères : 07/15/2021 00:00:00"
    }
}
```

Comme vous pouvez le voir dans l'exemple ci-dessus, la méthode ToString() est utilisée pour convertir une date en chaîne de caractères. Il existe également d'autres méthodes de conversion telles que ToShortDateString(), ToLongDateString(), ToShortTimeString() et ToLongTimeString() qui permettent d'obtenir différents formats de date et d'heure.

Pour une conversion plus précise, vous pouvez également spécifier le format de sortie souhaité en utilisant des codes de formatage. Par exemple :

```C#
string maDateEnChaine = maDate.ToString("dd/MM/yyyy");
// La sortie sera : "15/07/2021"
```

## Plongée Approfondie

La méthode ToString() est définie dans la classe de base Object, et donc elle peut être utilisée pour convertir n'importe quel objet en chaîne de caractères. Lorsqu'elle est utilisée pour convertir une date, elle utilise le format de date et d'heure par défaut pour le système actuel.

Cependant, si vous souhaitez utiliser un format différent, vous pouvez utiliser la méthode ToString() de la classe DateTime, qui comporte plusieurs surcharges pour spécifier le format souhaité. Vous pouvez également utiliser la classe CultureInfo pour spécifier des cultures différentes et obtenir des formats de date différents, notamment pour les langues étrangères.

Il est également important de noter qu'une fois qu'une date est convertie en chaîne de caractères, elle devient immuable. Cela signifie que la conversion ne modifie pas l'objet d'origine, mais crée plutôt une nouvelle chaîne de caractères. Il est donc recommandé de stocker la valeur convertie dans une variable séparée si vous devez ensuite la réutiliser dans votre code.

## Voir aussi

- [Documentation officielle sur la méthode ToString()](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.tostring)
- [Guide de formatage d'une date en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Documentation sur la classe CultureInfo](https://docs.microsoft.com/fr-fr/dotnet/api/system.globalization.cultureinfo)