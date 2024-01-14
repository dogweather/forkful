---
title:    "Elixir: Convertir une chaîne en minuscules"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Pourquoi

Tout programmeur est confronté à des tâches récurrentes, comme la conversion d'une chaîne de caractères en minuscules. Mais pourquoi est-il important de pouvoir le faire ? Tout simplement parce que cela facilite la comparaison entre les chaînes de caractères, car la casse peut affecter les résultats. De plus, de nombreuses bases de données et langages de programmation sont sensibles à la casse, il est donc crucial de pouvoir convertir des chaînes en minuscules pour éviter des erreurs lors des opérations.

## Comment faire

Voici un exemple de code en Elixir pour convertir une chaîne en minuscules et afficher le résultat :

```Elixir
string = "Ici, nous convertissons une chaîne en minuscules."
puts String.downcase(string)
```

Résultat :

```Elixir
"ici, nous convertissons une chaîne en minuscules."
```

Comme vous pouvez le constater, la fonction `downcase` de la bibliothèque standard Elixir permet de convertir facilement une chaîne en minuscules. Cette fonction accepte également un deuxième argument qui spécifie la langue à utiliser pour la conversion. Par défaut, c'est la langue anglaise qui est utilisée, mais vous pouvez spécifier d'autres langues comme le français ou l'espagnol.

## Plongée en profondeur

Pour comprendre comment la fonction `downcase` fonctionne, il est important de connaître les concepts de base de la programmation fonctionnelle. En Elixir, les chaînes de caractères sont des listes de nombres correspondant aux codes ASCII des caractères. Ainsi, pour convertir une chaîne en minuscules, il suffit de parcourir cette liste et d'appliquer la fonction `String.downcase` à chaque code de caractère.

Un autre élément à prendre en compte est que la fonction `downcase` ne modifie pas la chaîne d'origine, mais renvoie une nouvelle chaîne en minuscules. Cela est dû au principe de l'immutabilité en Elixir, où les données ne peuvent pas être modifiées une fois qu'elles ont été créées.

# Voir aussi

- [Documentation officielle Elixir pour la fonction downcase](https://hexdocs.pm/elixir/String.html#downcase/2)
- [Article sur l'immutabilité en Elixir](https://medium.com/elixir-mastery/the-concept-of-immutability-in-elixir-f3629a60659b)
- [Tutoriel sur la programmation fonctionnelle en Elixir](https://elixir-lang.org/getting-started/introduction.html#functional-programming)