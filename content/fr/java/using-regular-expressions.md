---
title:                "Java: Utilisation des expressions régulières"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant en programmation qui vous permettent de rechercher et de manipuler des chaînes de caractères selon des motifs spécifiques. Ils peuvent être utilisés pour valider des entrées utilisateur, rechercher des données dans une base de données ou encore formater du texte de manière précise. En bref, ils peuvent grandement aider à simplifier et à automatiser des tâches dans le code.

## Comment faire

```Java
// Exemple de code pour vérifier si une adresse email est valide en utilisant une expression régulière
String email = "johndoe@gmail.com";
String regex = "^[A-Za-z0-9+_.-]+@(.+)$";
Pattern pattern = Pattern.compile(regex);
Matcher matcher = pattern.matcher(email);
if(matcher.matches()){
    System.out.println("Adresse email valide !");
}else{
    System.out.println("Adresse email invalide!");
}
```
Résultat :
> Adresse email valide !

Dans cet exemple, nous utilisons des caractères spéciaux comme `^` pour indiquer le début de la chaîne, `+` pour indiquer qu'un ou plusieurs caractères peuvent être présents et `$` pour indiquer la fin de la chaîne. Le résultat final est une expression régulière qui vérifie si l'email donnée est sous la forme d'une adresse valide.

## Plongée en profondeur

Les expressions régulières peuvent sembler intimidantes au premier abord, mais il est important de se familiariser avec leur syntaxe pour en comprendre toute leur puissance. Voici quelques conseils pour utiliser efficacement les expressions régulières en Java :

- Utilisez les méthodes `Pattern.compile()` et `Matcher.matcher()` pour compiler et mettre en correspondance votre expression régulière.
- Utilisez les classes `Pattern` et `Matcher` pour rechercher et manipuler des correspondances avec des caractères spécifiques.
- Expérimentez avec les caractères spéciaux tels que `*` pour correspondre à zéro ou plusieurs occurrences d'un caractère, `?` pour un caractère en option, ou encore `|` pour une alternative entre deux motifs.
- N'oubliez pas d'utiliser des échappements (`\`) pour utiliser des caractères spéciaux littéralement si besoin, par exemple `"\\d"` pour correspondre à un chiffre.

Pour plus d'informations sur les expressions régulières en Java, consultez la documentation officielle de Java ou explorez des sites tels que Regex101 qui propose un éditeur interactif pour vous aider à tester et à comprendre vos expressions régulières.

See Also
- [Documentation Java sur Pattern](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Documentation Java sur Matcher](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html)
- [Regex101](https://regex101.com/) (en anglais)