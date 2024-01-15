---
title:                "Effacement de caractères correspondants à un modèle"
html_title:           "Ruby: Effacement de caractères correspondants à un modèle"
simple_title:         "Effacement de caractères correspondants à un modèle"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut y avoir différentes raisons pour lesquelles vous voudriez supprimer des caractères correspondant à un motif particulier dans votre code Ruby. Peut-être que vous essayez de nettoyer une chaîne de caractères en retirant les caractères spéciaux, ou peut-être que vous voulez vérifier qu'une chaîne de caractères ne contient que des chiffres.

## Comment faire

Dans Ruby, il existe plusieurs façons de supprimer des caractères correspondant à un motif. Voici quelques exemples :

```Ruby
# Supprimer tous les espaces d'une chaîne de caractères
str = "Ruby est super cool !"
str.delete!(" ")

# Supprimer tous les chiffres d'une chaîne de caractères
str = "12345678"
str.delete!("0-9")

# Supprimer tous les caractères spéciaux d'une chaîne de caractères
str = "Le projet Ruby a débuté le 24 février 1993."
str.delete!("/[^A-Za-z0-9]/") # l'espace après "delete!" est important
```

Output:

```bash
Rbystrsuprcol!
  +
-
LeprojetRubya24fvrier1993
```

## Plongée en profondeur

Ruby offre de nombreuses méthodes pour supprimer des caractères correspondant à un motif, notamment `delete`, `delete_if`, `gsub`, `tr`, `tr_s`, `squeeze`, `squeeze!` et `strip`. Selon vos besoins, vous pouvez utiliser ces méthodes pour supprimer des caractères spécifiques en fonction d'une condition, effectuer un remplacement, ou même supprimer les caractères répétés dans une chaîne de caractères.

## Voir aussi

Voici quelques ressources utiles pour en savoir plus sur les méthodes de suppression de caractères dans Ruby :

- [La documentation officielle de Ruby sur les méthodes de suppression de caractères](https://ruby-doc.org/core/classes/String.html#M000355)
- [Un tutoriel sur la manipulation de chaînes de caractères en Ruby](https://www.rubyguides.com/2015/05/manipulating-strings-in-ruby/)
- [Un article sur le filtrage de caractères en utilisant `gsub` dans Ruby](https://www.rubyguides.com/2019/02/ruby-gsub-method/)