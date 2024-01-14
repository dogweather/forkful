---
title:    "Java: Convertir une chaîne en minuscules"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Pourquoi:

La conversion d'une chaîne de caractères en minuscules est une tâche courante en programmation Java. Elle est utile pour le traitement de données telles que les noms d'utilisateurs ou les mots de passe, ainsi que pour la comparaison de chaînes dans les algorithmes de recherche et tri. Dans cet article, nous allons plonger dans les détails de cette opération.

Comment faire:

Pour convertir une chaîne de caractères en minuscules en Java, vous pouvez utiliser la méthode `toLowerCase()` de la classe `String`. Voici un exemple de code pour cela:

```java
String str = "Bonjour";
String newStr = str.toLowerCase(); // newStr vaut maintenant "bonjour"
System.out.println(newStr);
```

La méthode `toLowerCase()` renvoie une nouvelle chaîne avec tous les caractères en minuscules. Ainsi, si vous avez besoin de modifier la chaîne d'origine, vous devrez assigner la nouvelle chaîne à la variable d'origine. Si vous ne voulez pas créer une nouvelle chaîne, vous pouvez utiliser la méthode `StringBuffer` ou `StringBuilder` pour modifier la chaîne d'origine directement.

Profondeur de plongée:

Sous le capot, la méthode `toLowerCase()` utilise le tableau de codage Unicode du caractère correspondant pour effectuer la conversion. Cela signifie que cette méthode peut gérer les caractères accentués et spéciaux en plus des lettres de l'alphabet.

Cependant, il est important de noter que la conversion en minuscules dépend de la langue par défaut de votre système d'exploitation. Si vous utilisez des caractères accentués dans une langue différente, la conversion peut donner des résultats inattendus. Dans ce cas, il est préférable d'utiliser la méthode `toLowerCase(Locale)` en spécifiant la locale appropriée pour votre langue.

Voir aussi:

Voici quelques liens utiles pour en savoir plus sur la conversion d'une chaîne en minuscules en Java:

- [Documentation officielle de la classe String](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Tutoriel Java sur le traitement de texte](https://docs.oracle.com/javase/tutorial/i18n/text/intro.html)
- [Liste des locales en Java](https://docs.oracle.com/javase/6/docs/api/java/util/Locale.html)