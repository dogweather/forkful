---
title:    "Java: Convertir une chaîne en minuscules"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Pourquoi
Il est important de connaître comment convertir une chaîne de caractères en lettres minuscules en Java car cela peut vous aider à manipuler et à comparer facilement les chaînes de caractères dans vos programmes.

# Comment faire
Voici un exemple de code en Java pour convertir une chaîne de caractères en lettres minuscules : 

```Java
String str = "Bonjour le Monde!";
String lowercaseStr = str.toLowerCase(); 
System.out.println(lowercaseStr);
```
**Sortie** : bonjour le monde!

Dans cet exemple, nous utilisons la méthode `toLowerCase()` sur l'objet `str` pour convertir toutes les lettres en minuscules et stockons le résultat dans une nouvelle variable `lowercaseStr`. Ensuite, nous imprimons cette variable pour vérifier que la conversion a bien été effectuée.

Un autre moyen de convertir une chaîne en lettres minuscules est d'utiliser la méthode `toLowerCase(Locale)`. Cette méthode permet de spécifier une localisation pour la conversion. Voici un exemple de code utilisant cette méthode :

```Java
String str = "Bonjour le Monde!";
String lowercaseStr = str.toLowerCase(Locale.FRENCH); 
System.out.println(lowercaseStr);
```
**Sortie** : bonjour le monde!

Dans cet exemple, nous avons spécifié la localisation française pour la conversion en utilisant la constante `Locale.FRENCH`.

# Plongée en profondeur
La méthode `toLowerCase()` utilise la localisation par défaut de votre système lors de la conversion. Cela signifie que si votre système est configuré en anglais, la méthode convertira les lettres en minuscules selon les règles de la langue anglaise.

Cependant, il est possible de spécifier une localisation différente pour être plus précis dans la conversion. Par exemple, si vous voulez convertir des caractères avec des accents en lettres minuscules, il est recommandé d'utiliser la localisation appropriée pour cette langue.

Il est également important de noter que la méthode `toLowerCase()` ne change pas la valeur originale de la chaîne de caractères. Cela signifie que si vous avez besoin de la chaîne originale avec des lettres en minuscules, vous devrez l'assigner à une nouvelle variable comme nous l'avons fait dans les exemples précédents.

# Voir également
- [Documentation officielle de la classe String en Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutoriel sur les chaînes de caractères en Java](https://www.w3schools.com/java/java_strings.asp)
- [Localisations en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)