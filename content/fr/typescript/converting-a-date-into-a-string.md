---
title:    "TypeScript: Conversion d'une date en chaîne de caractères"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Convertir une date en une chaîne de caractères peut sembler être une tâche simple, mais cela peut être très utile lors de la manipulation de dates dans vos programmes TypeScript. En comprenant comment cela fonctionne, vous pourriez trouver de nombreuses utilisations pratiques pour cette compétence. Dans cet article, nous allons explorer comment convertir une date en une chaîne de caractères en TypeScript.

## Comment faire

Tout d'abord, nous devons définir une date en utilisant le constructeur Date de TypeScript :

```TypeScript
let date = new Date();
```

Ensuite, pour convertir cette date en une chaîne de caractères, nous pouvons utiliser la méthode toDateString() :

```TypeScript
let str = date.toDateString();
```

Dans cet exemple, la variable "str" contiendra maintenant la date sous forme de chaîne de caractères dans le format suivant : "Tue Jul 20 2021".

Nous pouvons également personnaliser le format en utilisant la méthode toLocaleDateString() :

```TypeScript
let str = date.toLocaleDateString('fr-FR', {dateStyle: 'full'});
```

Dans cet exemple, la variable "str" contiendra la date sous forme de chaîne de caractères dans un format complet en français, par exemple "mardi 20 juillet 2021".

## Plongée en profondeur

Maintenant que nous savons comment convertir une date en une chaîne de caractères en TypeScript, il est important de noter que cette méthode ne modifie pas l'objet Date d'origine, elle retourne simplement une nouvelle chaîne de caractères. De plus, il existe plusieurs options pour personnaliser le format de la chaîne de caractères, telles que la langue et le style de date.

## Voir aussi

- [Documentation officielle de TypeScript sur les objets Date](https://www.typescriptlang.org/docs/handbook/intro-date-time.html)
- [Exemples de conversion de dates en chaînes de caractères en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date/toString)