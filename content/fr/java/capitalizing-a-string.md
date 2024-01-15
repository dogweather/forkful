---
title:                "Majuscules d'une chaîne de caractères"
html_title:           "Java: Majuscules d'une chaîne de caractères"
simple_title:         "Majuscules d'une chaîne de caractères"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Java, vous pouvez vous demander pourquoi vous voudriez les capitaliser. Il y a plusieurs raisons pour lesquelles vous pourriez vouloir le faire, comme standardiser vos données ou les rendre plus lisibles pour les utilisateurs.

## Comment faire

Pour capitaliser une chaîne de caractères en Java, vous pouvez utiliser la méthode `toUpperCase()` de la classe `String`. Voici un exemple de code :

```Java
String str = "Exemple de chaîne de caractères";
String capitalizedStr = str.toUpperCase();

System.out.println(capitalizedStr);
```

Cela produira la sortie suivante :

```Java
EXEMPLE DE CHAÎNE DE CARACTÈRES
```

Vous pouvez également utiliser la méthode `toLowerCase()` si vous préférez convertir la chaîne en minuscules.

## Plongée en profondeur

La méthode `toUpperCase()` utilise les règles d’Unicode pour convertir les caractères de la chaîne en majuscules. Cela signifie qu'elle fonctionnera correctement pour les caractères spéciaux et les lettres accentuées. Vous pouvez aussi utiliser la méthode `toUpperCase(Locale)` pour spécifier une locale spécifique pour la conversion.

Il est important de noter que la méthode `toUpperCase()` ne modifie pas la chaîne originale, mais retourne une nouvelle chaîne avec les caractères en majuscules. Si vous voulez modifier directement la chaîne d'origine, vous pouvez utiliser la méthode `toUpperCase(Locale,StringBuffer,String)`, en fournissant en plus un objet `StringBuffer` pour stocker la chaîne modifiée.

## Voir aussi

- [Documentation officielle de la méthode `toUpperCase()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Documentation officielle des locales en Java](https://docs.oracle.com/javase/tutorial/i18n/locale/index.html)