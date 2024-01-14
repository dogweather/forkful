---
title:    "Elixir: Concaténation de chaînes de caractères"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes lors de la programmation est la concaténation de chaînes de caractères. Cela consiste à combiner plusieurs chaînes pour en former une seule. Mais pourquoi est-ce utile? Eh bien, la concaténation de chaînes peut être utilisée pour améliorer la lisibilité de votre code en le rendant plus clair et plus compréhensible pour les autres développeurs. De plus, cela peut être utile pour créer des messages ou des en-têtes personnalisés dans votre programme.

## Comment faire

La concaténation de chaînes peut être accomplie en utilisant l'opérateur `<>` ou en appelant la fonction `String.concat/2` dans Elixir. Voyons quelques exemples pour mieux comprendre.

Voici un exemple utilisant l'opérateur `<>`:

```Elixir
"Bonjour" <> "tout le monde!" 
```

Output: Bonjour tout le monde!

Et voici un autre exemple utilisant la fonction `String.concat/2`:

```Elixir
String.concat(["Bonjour ", "à ", "tous!"]) 
```

Output: Bonjour à tous!

Comme vous pouvez le voir, nous pouvons utiliser l'une ou l'autre de ces méthodes pour concaténer des chaînes de caractères. Cependant, la fonction `String.concat/2` peut être utile si vous voulez concaténer un grand nombre de chaînes.

## Plongée en profondeur

Elixir utilise des listes pour stocker les chaînes de caractères. Cela signifie que chaque chaîne est en réalité une liste de caractères. Lorsque nous concaténons deux chaînes, Elixir crée une nouvelle liste contenant les caractères des deux chaînes et la renvoie en tant que chaîne de caractères. Cependant, si vous concaténez une grande quantité de chaînes, cela peut affecter les performances de votre programme.

Il est également important de noter que lors de la concaténation de chaînes, Elixir remplace toutes les listes restantes par une seule liste après l'opération. Cela peut également avoir un impact sur les performances de votre code si la concaténation est effectuée à plusieurs reprises.

## Voir aussi

Si vous souhaitez en savoir plus sur la manipulation des chaînes de caractères en Elixir, vous pouvez consulter ces liens :

- [Documentation sur la concaténation de chaînes en Elixir](https://hexdocs.pm/elixir/String.html#concat/2)
- [Tutoriel vidéo sur la concaténation de chaînes en Elixir](https://www.youtube.com/watch?v=ZA5bFS0Gfqk)
- [Discussion sur les performances de la concaténation de chaînes en Elixir](https://stackoverflow.com/questions/2149059/string-concatenation-performance-in-elixir)

Maintenant, vous devriez avoir une meilleure compréhension de la concaténation de chaînes en Elixir et comment l'utiliser efficacement dans votre code. Amusez-vous à créer des chaînes personnalisées dans vos programmes!