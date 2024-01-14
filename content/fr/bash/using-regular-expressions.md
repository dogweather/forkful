---
title:    "Bash: Utiliser des expressions régulières"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Bash, alors vous savez que la manipulation de chaînes de caractères peut parfois être fastidieuse. Cependant, en utilisant des expressions régulières, vous pouvez écrire des patterns pour rechercher et manipuler ces chaînes de caractères de manière efficace. Cela peut vous faire gagner beaucoup de temps et rendre votre code plus propre et plus facile à maintenir.

## Comment faire

Pour utiliser des expressions régulières en Bash, vous devez utiliser l'outil de recherche de motif `grep` avec l'option `-E` qui permet d'activer les expressions régulières. Voici un exemple de code utilisant une expression régulière pour rechercher toutes les adresses e-mail dans un fichier :

```Bash
fichier="emails.txt"

grep -E "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}" $fichier
 ```
 
Cet exemple utilise une expression régulière couramment utilisée pour trouver des adresses e-mail valides. Le résultat affichera toutes les adresses e-mail qui correspondent à cette expression régulière.

## Plongeons plus en détail

Les expressions régulières peuvent sembler complexes au début, mais une fois que vous aurez compris la syntaxe, elles deviendront un outil très puissant pour la manipulation de chaînes de caractères. Voici quelques éléments clés à retenir :

- Les caractères spéciaux tels que `^`, `$`, `.`, `*` ont des significations spéciales dans les expressions régulières.

- Les classes de caractères, telles que `[A-Za-z]` pour les lettres de l'alphabet, `[0-9]` pour les chiffres et `[A-Za-z0-9]` pour les lettres et les chiffres peuvent être utilisées pour cibler des caractères spécifiques.

- Les quantificateurs, tels que `+` pour un ou plusieurs et `*` pour zéro ou plusieurs, peuvent être utilisés pour définir la quantité de caractères à rechercher.

Pour en savoir plus sur les expressions régulières, vous pouvez consulter la documentation de `grep` ou faire des recherches sur Internet pour trouver des tutoriels et des exemples plus avancés.

## Voir aussi

- [`grep` documentation](https://www.gnu.org/software/grep/manual/grep.html)

- [Regex tutorial](https://www.regular-expressions.info/tutorial.html)

- [Regex cheat sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)