---
title:                "Supprimer les caractères correspondant à un schéma"
html_title:           "C#: Supprimer les caractères correspondant à un schéma"
simple_title:         "Supprimer les caractères correspondant à un schéma"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi
Supprimer des caractères correspondant à un motif peut être utile lorsqu'on veut nettoyer des données en supprimant des éléments indésirables, comme des symboles ou des espaces, d'une chaîne de caractères.

## Comment faire
Pour supprimer des caractères correspondant à un motif dans une chaîne de caractères en C#, nous pouvons utiliser la méthode `Regex.Replace()` et spécifier le motif à supprimer dans le premier argument et une chaîne vide `""` dans le deuxième argument.

Voici un exemple concret de code qui supprime tous les symboles (caractères non alphabétiques) d'une chaîne de caractères :

```
string phrase = "J'aime ! écrire en C# ~";
string motif = "[^a-zA-Z]";
string resultat = Regex.Replace(phrase, motif, "");
Console.WriteLine(resultat);
```

Cela produira une sortie : `J'aime crireenC`.

Nous pouvons également utiliser des expressions régulières pour supprimer des motifs plus spécifiques. Par exemple, si nous voulons supprimer tous les espaces d'une chaîne :

```
string phrase = "Je suis une phrase avec des espaces";
string motif = @"\s+";
string resultat = Regex.Replace(phrase, motif, "");
Console.WriteLine(resultat);
```

Cela produira une sortie : `Jesuisunephraseavecdesespaces`.

## Plongée approfondie
En utilisant la méthode `Regex.Replace()`, nous pouvons également utiliser des options pour préciser la façon dont nous voulons que notre motif corresponde aux caractères de la chaîne. Par exemple, en ajoutant l'option `RegexOptions.IgnoreCase`, nous pouvons nous assurer que notre motif supprimera les caractères dans une chaîne, quel que soit leur casse. Voici un exemple :

```
string phrase = "J'aime écrire en C#";
string motif = "e";
string resultat = Regex.Replace(phrase, motif, "", RegexOptions.IgnoreCase);
Console.WriteLine(resultat);
```

Cela produira une sortie : `J'aim écrir n C#`.

De plus, en utilisant l'option `RegexOptions.Multiline`, nous pouvons spécifier que notre motif doit être appliqué sur chaque ligne de la chaîne, plutôt que sur toute la chaîne. Cela peut être utile lors du nettoyage de données avec plusieurs lignes. Voici un exemple :

```
string texte = @"Première ligne
Deuxième ligne
Troisième ligne";
string motif = @"\n";
string resultat = Regex.Replace(texte, motif, ", ", RegexOptions.Multiline);
Console.WriteLine(resultat);
```

Cela produira une sortie : `Première ligne, Deuxième ligne, Troisième ligne`.

## Voir également
- La documentation officielle sur les expressions régulières en C# : https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference
- Un tutoriel sur les expressions régulières en C# : https://www.dotnetperls.com/regex-match