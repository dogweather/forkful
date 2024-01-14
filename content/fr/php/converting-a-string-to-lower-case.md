---
title:    "PHP: Transformer une chaîne de caractères en minuscules"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous qu'il existe une fonction en PHP qui permet de convertir une chaîne de caractères en minuscules ? Dans cet article, nous allons examiner pourquoi il peut être utile d'utiliser la fonction `strtolower()` et comment l'utiliser correctement.

## Comment faire

La fonction `strtolower()` prend une chaîne de caractères en entrée et renvoie une nouvelle chaîne avec tous les caractères en minuscules. Voyons cela en action avec un exemple de code :

```PHP
$string = "Bonjour le Monde!";
echo strtolower($string);
```

Lorsque nous exécutons ce code, nous obtenons le résultat suivant :

```
bonjour le monde!
```

Comme vous pouvez le voir, la fonction a converti tous les caractères en minuscules, ce qui peut être utile pour des tâches telles que trier des mots par ordre alphabétique.

## Plongée en profondeur

Lorsque vous utilisez la fonction `strtolower()`, il est important de noter que cela peut varier en fonction de la langue et de l'encodage de votre texte. Par exemple, si vous travaillez avec des caractères spéciaux ou des lettres accentuées, vous devrez peut-être utiliser une fonction différente pour obtenir des résultats précis.

Il est également important de tenir compte de la sensibilité à la casse de votre code. Cela signifie que si votre code est sensible à la casse, les lettres majuscules et minuscules seront considérées comme différentes, même après avoir utilisé `strtolower()`. Dans ce cas, vous devrez peut-être utiliser une autre méthode, telle que `strcasecmp()`, qui compare les chaînes de caractères sans tenir compte de la casse.

## Voir aussi

Pour en savoir plus sur les fonctions de manipulation de chaînes de caractères en PHP, vous pouvez consulter les ressources suivantes :

- [La documentation officielle sur `strtolower()`](https://www.php.net/manual/fr/function.strtolower.php)
- [Une liste complète des fonctions de manipulation de chaînes en PHP](https://www.php.net/manual/fr/ref.strings.php)
- [Un tutoriel sur les chaînes de caractères en PHP](https://www.w3schools.com/php/php_strings.asp)

Maintenant que vous connaissez la fonction `strtolower()`, vous pouvez l'utiliser dans vos projets PHP pour faciliter la manipulation de chaînes de caractères en minuscules. N'hésitez pas à l'essayer dans vos propres codes et à découvrir d'autres fonctions utiles pour travailler avec les chaînes de caractères en PHP !