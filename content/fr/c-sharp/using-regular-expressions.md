---
title:                "Utilisation des expressions régulières"
html_title:           "C#: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi l'utiliser?

Les expressions régulières sont un outil puissant utilisé par les programmeurs pour rechercher et manipuler des motifs spécifiques dans du texte. Elles sont couramment utilisées pour valider les entrées utilisateur, extraire des données d'un texte et réaliser des modifications complexes sur des chaînes de caractères. Les programmeurs utilisent régulièrement les expressions régulières car elles permettent d'automatiser des tâches fastidieuses et d'économiser du temps et des efforts.

## Comment faire:

Voici un exemple de code en C# montrant comment utiliser les expressions régulières pour extraire des numéros de téléphone à partir d'une chaîne de caractères :

```C#
string texte = "Mon numéro de téléphone est le (123) 456-7890.";
string motif = @"\(\d{3}\) \d{3}-\d{4}";

MatchCollection correspondances = Regex.Matches(texte, motif);

foreach (Match correspondance in correspondances)
{
   Console.WriteLine(correspondance.Value);
}

// Résultat:
// (123) 456-7890
```

Dans cet exemple, la variable "texte" contient la chaîne de caractères dans laquelle nous recherchons des numéros de téléphone. La variable "motif" définit le modèle que nous recherchons, en utilisant des caractères spéciaux comme "\d" pour représenter un chiffre et des quantificateurs comme "{3}" pour indiquer le nombre de fois qu'un motif doit apparaître. Nous utilisons ensuite la méthode Match pour trouver toutes les correspondances dans la chaîne de caractères et les stocker dans une collection. Enfin, nous parcourons cette collection et affichons les correspondances trouvées.

## Plongez plus en profondeur:

Les expressions régulières ont été inventées par le scientifique américain Stephen Kleene dans les années 1950 et se sont imposées comme un outil essentiel pour les programmeurs dès les années 1980. Bien qu'elles soient couramment utilisées en programmation, il existe d'autres alternatives pour effectuer des opérations sur du texte, telles que les méthodes de la classe String en C# et les outils de manipulation de texte intégrés dans les éditeurs de texte.

En utilisant les expressions régulières, il est également possible de personnaliser la casse, d'ignorer les caractères non-alphanumériques ou de rechercher des motifs à l'aide d'opérateurs logiques, offrant ainsi encore plus de flexibilité lors de la manipulation de texte.

## Voir aussi:

Pour en savoir plus sur l'utilisation des expressions régulières en C#, consultez la documentation officielle de Microsoft : [Expressions régulières en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference).

Si vous souhaitez vous familiariser avec les expressions régulières avant de les utiliser en programmation, vous pouvez utiliser des sites en ligne comme [Regex101](https://regex101.com/) pour tester vos modèles et voir des explications détaillées sur leur fonctionnement.