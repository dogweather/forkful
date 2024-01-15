---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "Go: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être une tâche utile lors de la manipulation de chaînes de caractères dans un programme Go. Cela peut être nécessaire pour effectuer des opérations telles que nettoyer des saisies utilisateur ou extraire des données spécifiques d'une chaîne plus grande.

## Comment faire

Voici comment supprimer des caractères correspondant à un motif en utilisant la méthode `ReplaceAllString()` de la bibliothèque `regexp` de Go:

```
regexp := regexp.MustCompile("motif") 
texte := "exemple de texte avec motif et plus de motif"
nouveauTexte := regexp.ReplaceAllString(texte, "")
```

Le résultat sera une chaîne de caractères avec tous les motifs supprimés:
```
"exemple de texte avec et plus de"
```

## Plongée en profondeur

La méthode `ReplaceAllString()` utilise des expressions régulières pour rechercher et remplacer les motifs dans une chaîne. Elle prend en compte les caractères spéciaux tels que les astérisques ou les points d'interrogation dans la recherche de motifs.

Il est également possible d'utiliser la méthode `ReplaceAll()` de la bibliothèque `strings` pour supprimer des caractères correspondant à un motif spécifique dans une chaîne.

## Voir aussi

Pour en savoir plus sur les méthodes de suppression de motifs en Go, vous pouvez consulter les ressources suivantes:

- [Documentation officielle de la bibliothèque regexp](https://pkg.go.dev/regexp)
- [Documentation officielle de la bibliothèque strings](https://pkg.go.dev/strings)
- [Guide de référence de Go pour les expressions régulières](https://gobyexample.com/regular-expressions)