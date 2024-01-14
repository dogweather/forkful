---
title:    "Elixir: Recherche et remplacement de texte"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi


La recherche et le remplacement de texte est une tâche courante dans la programmation, et cela peut être fait de manière efficace en utilisant Elixir. Dans cet article, nous allons expliquer pourquoi il est utile d'utiliser Elixir pour la recherche et le remplacement de texte.

## Comment faire?


Pour commencer, nous allons voir comment utiliser la fonction `String.replace` en Elixir pour rechercher et remplacer du texte dans une chaîne de caractères. Voici un exemple de code avec une explication du résultat :

```Elixir
# Création d'une chaîne de caractères contenant le mot "broccoli" deux fois
texte = "J'aime manger du broccoli, mais je n'aime pas sa texture."

# Utilisation de la fonction String.replace pour remplacer "broccoli" par "chou-fleur"
nouveau_texte = String.replace(texte, "broccoli", "chou-fleur")

# Affichage du nouveau texte
IO.puts(nouveau_texte)

# Résultat: "J'aime manger du chou-fleur, mais je n'aime pas sa texture."
```

Comme vous pouvez le constater, la fonction `String.replace` prend trois arguments : la chaîne de caractères à modifier, le motif à rechercher et le texte de remplacement. Elle retourne ensuite une nouvelle chaîne de caractères modifiée.

Il est également possible d'utiliser des expressions régulières pour effectuer des recherches plus complexes. Voici un exemple de code avec une expression régulière pour remplacer tous les chiffres dans une chaîne de caractères par un caractère `x` :

```Elixir
texte = "123 ABC 456 DEF"
nouveau_texte = Regex.replace(~r/[0-9]+/, texte, "x")

# Résultat : "x ABC x DEF"
```

## Plongée en profondeur


Maintenant que vous avez vu comment utiliser la fonction `String.replace` en Elixir, parlons un peu plus en détail de la recherche et du remplacement de texte.

En utilisant des expressions régulières, il est possible de capturer des parties du texte que vous recherchez et de les utiliser dans le texte de remplacement. Par exemple, si vous souhaitez remplacer le mot "chien" par "chat" tout en gardant la première lettre en majuscule, vous pouvez utiliser une expression régulière avec une capture :

```Elixir
texte = "J'aime les chiens, mais je préfère les chats."
nouveau_texte = Regex.replace(~r/\b(chien)\b/, texte, "\u$1")

# Résultat : "J'aime les Chiens, mais je préfère les Chats."
```

Il est également possible de réaliser des opérations de recherche et de remplacement sur des fichiers en utilisant les fonctions du module `File`. Vous pouvez ainsi automatiser des tâches telles que la modification de plusieurs fichiers en une seule fois.

## Voir aussi


- Documentation officielle d'Elixir sur la recherche et le remplacement de texte : https://hexdocs.pm/elixir/String.html#replace/4
- Tutoriel sur les expressions régulières en Elixir : https://elixirschool.com/fr/lessons/specifics/regex/