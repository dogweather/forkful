---
title:    "Fish Shell: Recherche et remplacement de texte."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation. Cela peut être utile lorsque vous souhaitez modifier plusieurs occurrences d'un mot ou d'une phrase dans votre code. Heureusement, le shell Fish offre des fonctionnalités intégrées pour faciliter cette tâche.

## Comment faire

Pour commencer, vous avez besoin d'un terminal avec Fish Shell installé sur votre système d'exploitation. Une fois que vous avez ouvert votre terminal, vous pouvez commencer à utiliser la commande intégrée "sed" pour effectuer des recherches et des remplacements de texte.

```Fish Shell
# Supposez que vous avez un fichier texte nommé "example.txt" avec du texte suivant
#This is an example string to be replaced

# Utilisez la commande "sed" pour remplacer toutes les occurrences de "example" par "fish"
sed -i 's/example/fish/' example.txt

# Vérifiez le fichier pour voir la modification
cat example.txt 

# Output: This is an fish string to be replaced
```

Vous pouvez également utiliser des expressions régulières pour effectuer des recherches et des remplacements plus avancés. Par exemple, si vous souhaitez remplacer toutes les occurrences d'adresses email dans un fichier par "xxx@xxx.com", vous pouvez utiliser la commande suivante:

```Fish Shell
# Supposez que vous avez un fichier texte nommé "emails.txt" avec des adresses email
# Utilisez la commande "sed" avec une expression régulière pour remplacer toutes les adresses par "xxx@xxx.com"
sed -i 's/\w+@\w+\.com/xxx@xxx.com/g' emails.txt

# Vérifiez le fichier pour voir la modification
cat emails.txt

# Output: xxx@xxx.com
```

## Plongée en profondeur

La commande "sed" peut sembler un peu intimidante pour les débutants, mais une fois que vous comprenez comment elle fonctionne, vous pouvez effectuer des recherches et des remplacements de texte puissants avec facilité. Voici quelques points à retenir:

- La commande "sed" utilise la syntaxe "s/old_text/new_text/" pour effectuer un remplacement simple.
- Vous pouvez ajouter un "g" à la fin de la commande pour remplacer toutes les occurrences d'une chaîne de caractères. Sans cela, seule la première occurrence sera remplacée.
- Les expressions régulières peuvent être utilisées pour effectuer des recherches plus avancées et des remplacements ciblés.
- Assurez-vous de bien comprendre votre expression régulière avant de l'utiliser pour éviter des modifications inattendues dans votre fichier.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/)
- [Guide sur les expressions régulières en français](https://openclassrooms.com/fr/courses/918836-initiez-vous-au-bash/912833-utilisez-et-analysez-les-expressions-regulieres)