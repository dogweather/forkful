---
title:                "Fish Shell: Assemblage de chaînes de caractères"
simple_title:         "Assemblage de chaînes de caractères"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur ou un amateur de ligne de commande, vous avez probablement déjà entendu parler de la manipulation de chaînes de caractères. Mais pourquoi vouloir concaténer des chaînes de caractères ? Eh bien, la concaténation de chaînes de caractères est un outil utile pour combiner plusieurs chaînes de caractères ensemble afin de créer une seule chaîne plus longue. Cela peut être utile pour créer des messages dynamiques, des requêtes d'API ou même simplement du texte pour un affichage.

## Comment faire

La manipulation de chaînes de caractères peut sembler intimidante au premier abord, mais ne vous inquiétez pas, le Fish Shell rend cela très simple. Voyons voir à quoi ressemble la concaténation de chaînes de caractères en pratique.

```Fish Shell
set nom "Jean"
set age 23
set phrase "Bonjour, je m'appelle "$nom" et j'ai "$age" ans."
echo $phrase
```

Dans cet exemple, nous avons défini trois variables : "nom", "age" et "phrase". Ensuite, nous avons utilisé le symbole "$" pour concaténer les variables dans la chaîne de caractères "phrase". Enfin, nous avons imprimé la phrase à l'aide de la commande "echo". Le résultat sera "Bonjour, je m'appelle Jean et j'ai 23 ans.".

Mais que se passe-t-il si vous voulez ajouter un point d'exclamation à la fin de la phrase ? Pas de problème, vous pouvez utiliser l'opérateur "+", comme ceci :

```Fish Shell
set phrase $phrase"!"
echo $phrase
```

Maintenant, la variable "phrase" contient "Bonjour, je m'appelle Jean et j'ai 23 ans !".

## Plongée en profondeur

Maintenant que vous savez comment concaténer des chaînes de caractères de base, parlons un peu plus en détail. Premièrement, il est important de noter que les chaînes de caractères entourées de guillemets doubles (" ") sont évaluées, tandis que les chaînes de caractères entourées de guillemets simples (' ') ne le sont pas. Cela signifie que pour concaténer une chaîne de caractères avec une variable, vous devez utiliser des guillemets doubles pour que la variable soit évaluée.

Une autre chose importante à savoir est que vous pouvez également utiliser l'opérateur de substitution "${ }" pour concaténer des chaînes de caractères. Cela peut être utile si vous avez besoin d'utiliser des variables avec des noms plus complexes ou des caractères spéciaux.

Enfin, il existe de nombreuses autres commandes et opérateurs pour manipuler les chaînes de caractères dans le Fish Shell, comme "string sub", "string join" ou encore l'utilisation de la syntaxe des chaînes de caractères héritée du langage de programmation C.

## Voir aussi

- [Documentation officielle de Fish Shell sur la manipulation de chaînes de caractères](https://fishshell.com/docs/current/tutorial.html#tut_strings)
- [Un tutoriel sur la concaténation de chaînes de caractères en Fish Shell](https://blog.bitsrc.io/string-concatenation-in-fish-shell-875ab728adcb)
- [Un guide avancé sur la manipulation de chaînes de caractères en ligne de commande](https://www.freecodecamp.org/news/working-with-strings-in-bash/)

Merci d'avoir lu cet article sur la concaténation de chaînes de caractères en Fish Shell. Nous espérons que vous avez trouvé ces informations utiles et que vous pourrez les appliquer dans vos projets futurs. N'hésitez pas à consulter nos autres tutoriels sur le Fish Shell pour en apprendre davantage !