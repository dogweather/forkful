---
date: 2024-01-20 17:34:12.534016-07:00
description: "La concat\xE9nation de cha\xEEnes, c'est simplement coller des bouts\
  \ de texte ensemble. On le fait souvent pour g\xE9n\xE9rer des messages, combiner\
  \ des donn\xE9es, ou\u2026"
lastmod: '2024-02-25T18:49:54.506915-07:00'
model: gpt-4-1106-preview
summary: "La concat\xE9nation de cha\xEEnes, c'est simplement coller des bouts de\
  \ texte ensemble. On le fait souvent pour g\xE9n\xE9rer des messages, combiner des\
  \ donn\xE9es, ou\u2026"
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La concaténation de chaînes, c'est simplement coller des bouts de texte ensemble. On le fait souvent pour générer des messages, combiner des données, ou construire dynamiquement du code.

## Comment faire :

```C#
string prenom = "Jean";
string message = "Bonjour, " + prenom + "!";

// Utiliser StringBuilder pour la concaténation dans les boucles
var sb = new System.Text.StringBuilder();
for(int i = 0; i < 3; i++) {
    sb.Append(prenom).Append("!");
}
string resultatsBoucle = sb.ToString();

// Affichage
Console.WriteLine(message);  // Affiche: Bonjour, Jean!
Console.WriteLine(resultatsBoucle); // Affiche: Jean!Jean!Jean!
```

## Plongée profonde

Historiquement, concaténer avec l'opérateur `+` était simple mais risquait de mener à de mauvaises performances dans des boucles ou des scénarios complexes. C'est là qu'intervient `StringBuilder` : il est conçu spécialement pour les concaténations répétitives et gère mieux la mémoire sous le capot. Depuis C# 6.0, il y a aussi l'interpolation de chaînes avec `$""`, qui rend le code plus lisible et plus court.

Alternativement, on a `String.Concat` et `String.Format` dans notre boîte à outils, qui sont utiles dans certaines situations. Par exemple, `String.Concat` est génial pour joindre des listes de chaînes sans séparateur. `String.Format` peut être plus lisible quand il y a beaucoup de variables à insérer dans un modèle de chaîne.

## Voir aussi

- Documentation Microsoft sur `StringBuilder`: https://docs.microsoft.com/fr-fr/dotnet/api/system.text.stringbuilder?view=net-6.0
- Guide sur l'interpolation de chaînes en C#: https://docs.microsoft.com/fr-fr/dotnet/csharp/language-reference/tokens/interpolated
- Explications sur `String.Format`: https://docs.microsoft.com/fr-fr/dotnet/api/system.string.format?view=net-6.0
