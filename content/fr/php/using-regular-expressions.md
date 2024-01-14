---
title:    "PHP: Utilisation des expressions régulières"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Why

Les expressions régulières sont un outil puissant dans la boîte à outils de tout programmeur PHP. Elles permettent de rechercher et de manipuler des chaînes de caractères avec une grande précision et facilité. Si vous cherchez à améliorer vos compétences en programmation et à gagner du temps dans vos projets, les expressions régulières sont un must.

##Comment faire

L'utilisation des expressions régulières en PHP peut sembler intimidante au premier abord, mais une fois que vous maîtrisez les bases, les possibilités sont infinies. Voici quelques exemples de code pour vous aider à démarrer :

```PHP
// Rechercher un mot spécifique dans une chaîne de caractères
$string = "Bonjour à tous!";
$searchWord = "bonjour";
if (preg_match("/$searchWord/i", $string)) {
    echo "Le mot $searchWord a été trouvé!";
} else {
    echo "Le mot $searchWord n'a pas été trouvé.";
}

// Remplacer tous les espaces par des tirets dans une URL
$url = "www.monsite.com/nouvel_article.html";
$newUrl = preg_replace("/\s+/", "-", $url);
echo "L'URL formattée est : $newUrl";
```

Output:
```
Le mot bonjour a été trouvé!
L'URL formattée est : www.monsite.com/nouvel-article.html
```

##Plongée en profondeur

Les expressions régulières en PHP utilisent une syntaxe spécifique pour définir des motifs de recherche. En voici quelques-unes des plus couramment utilisées :

- `.` : correspond à n'importe quel caractère
- `\d` : correspond à un chiffre
- `\s` : correspond à un espace
- `+` : correspond à un ou plusieurs caractères
- `*` : correspond à zéro ou plusieurs caractères
- `^` : correspond au début d'une chaîne de caractères
- `$` : correspond à la fin d'une chaîne de caractères

En outre, il est possible d'utiliser des crochets pour spécifier une liste de caractères possibles, ainsi que des accolades pour spécifier une quantité précise de caractères.

Il est important de noter que les expressions régulières peuvent également être combinées avec d'autres fonctions de manipulation de chaînes de caractères en PHP, telles que `preg_match_all()` et `preg_split()`.

##Voir aussi

- [Documentation officielle de PHP sur les expressions régulières](https://www.php.net/manual/fr/book.pcre.php)
- [Tutoriel vidéo sur les expressions régulières en PHP](https://www.youtube.com/watch?v=v_1Isn8pD9E)
- [Exemples de motifs de recherche courants avec explications](https://www.regular-expressions.info/tutorial.html)