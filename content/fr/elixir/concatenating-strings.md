---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

La concaténation de chaînes de caractères consiste à les réunir bout à bout pour former une nouvelle phrase. Les programmeurs la font pour simplifier la présentation et la modification des données textuelles.

## Comment faire:

Regardons quelques exemples de la façon dont vous pouvez concaténer des chaînes en Elixir avec l'opérateur `<>`.

```Elixir
chaine1 = "Bonjour, "
chaine2 = "monde!"
chaine = chaine1 <> chaine2
IO.puts(chaine)
```
Le résultat sera : ```Bonjour, monde!```

## Plongeons plus en profondeur

Historiquement, la concaténation de chaînes est une pratique courante en programmation depuis le développement des premiers langages de programmation. Elixir gère ceci en utilisant l'opérateur de concaténation `<>`.

Toutefois, il est intéressant de noter qu'il existe des alternatives à l'utilisation du `<>`. Par exemple, vous pouvez utiliser l'interpolation de chaîne qui permet d'intégrer une valeur dans une chaîne. 

```Elixir
chaine3 = "Salut, #{chaine2}"
IO.puts(chaine3)
```
Le résultat obtenu est : ```Salut, monde!```

En Elixir, la concaténation de chaînes avec `<>` est une simple utilisation de l'opérateur `Kernel.concat/2`. Il est également optimisé pour la concision et la vitesse.

## Voir aussi

Pour davantage d'informations et de ressources sur la concatenation des chaînes en Elixir, jettons un coup d'oeil à ces liens :

- [La documentation officielle de Elixir](https://elixir-lang.org/getting-started/basic-types.html#strings)
- [Un guide pratique sur la concaténation en Elixir](https://elixirschool.com/fr/lessons/basics/strings/)
- [Post de blog sur la concaténation en Elixir](https://learningelixir.joekain.com/concatenate-strings-in-elixir/)