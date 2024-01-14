---
title:    "Elixir: Utilisation des expressions régulières"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi Utiliser les Expressions Régulières en Elixir?

Les expressions régulières sont un outil très puissant pour la manipulation de chaînes de caractères en Elixir. Elles permettent de trouver et de remplacer des motifs spécifiques dans une chaîne de caractères, ce qui peut être utile dans de nombreuses situations de programmation. Dans cet article, nous allons voir comment utiliser les expressions régulières en Elixir et plonger plus en profondeur dans leur fonctionnement.

## Comment Utiliser les Expressions Régulières en Elixir?

Pour utiliser les expressions régulières en Elixir, nous devons d'abord importer le module `Regex` en utilisant la directive `import`. Ensuite, nous pouvons utiliser la fonction `match?/2` du module `Regex` pour vérifier si une chaîne de caractères correspond à un motif spécifique. Voici un exemple de code utilisant les expressions régulières pour trouver si une chaîne de caractères contient des chiffres :

```Elixir
import Regex
sample_string = "Abc123"
match?(~r/\d+/, sample_string)
```

Ceci renverra `true` car la chaîne de caractères contient des chiffres. Nous pouvons également utiliser les expressions régulières pour extraire des parties spécifiques d'une chaîne de caractères en utilisant la fonction `scan/2` du module `Regex`. Voici un exemple de code qui extrait des adresses e-mail à partir d'une chaîne de caractères :

```Elixir
sample_string = "Mon adresse e-mail est test@test.com"
scan(~r/[a-zA-Z0-9_]+@[a-zA-Z]+\.[a-zA-Z]+/, sample_string)
```

Ceci renverra `["test@test.com"]` car la chaîne de caractères contient une adresse e-mail valide.

## Plongée en Profondeur sur les Expressions Régulières en Elixir

Les expressions régulières en Elixir utilisent la syntaxe PCRE (Perl Compatible Regular Expressions) et offrent de nombreuses fonctionnalités telles que les quantificateurs, les classes de caractères et les regroupements. Elles peuvent également être utilisées avec des options pour ignorer la casse ou prendre en compte des caractères multilignes.

Il est important de noter que les expressions régulières en Elixir sont basées sur des chaînes de caractères Unicode, ce qui signifie qu'elles peuvent être utilisées pour traiter des caractères spéciaux tels que les accents ou les caractères non latins.

Enfin, il est à noter qu'il existe des alternatives aux expressions régulières en Elixir, telles que le module `String.Lexer` qui fournit des fonctions spécifiques pour la manipulation de chaînes de caractères en utilisant des règles définies par l'utilisateur.

## Voir Aussi

- [Documentation officielle des Expressions Régulières en Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Article sur les Expressions Régulières en Français](https://fr.wikipedia.org/wiki/Expression_r%C3%A9guli%C3%A8re)
- [Article sur les Alternatives aux Expressions Régulières en Elixir](https://www.petecorey.com/blog/2019/08/19/the-abcs-of-alternative-string-processing-in-elixir-with-string-lexer/)