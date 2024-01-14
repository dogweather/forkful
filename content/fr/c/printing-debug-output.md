---
title:    "C: Affichage de la sortie de débogage"
keywords: ["C"]
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous écrivez un programme en langage C, il est souvent utile d'afficher des messages de débogage pour comprendre ce qui se passe à l'intérieur de votre code. Cela peut vous aider à identifier les erreurs et à les résoudre plus rapidement.

# Comment faire

Pour afficher des messages de débogage, vous pouvez utiliser la fonction `printf()`. Cette fonction prend en charge le formatage des données et peut être très utile pour afficher des variables et des messages explicatifs.

Voici un exemple de code pour afficher une variable `nombre` et un message de débogage :

```C
int nombre = 5;
printf("La valeur de nombre est %d\n", nombre);
```

Lorsque vous exécutez ce code, vous obtiendrez la sortie suivante :

```
La valeur de nombre est 5
```

Notez que `%d` est utilisé pour formater et afficher la variable `nombre`.

Vous pouvez également utiliser `printf()` pour afficher des messages de débogage en cas d'erreur. Par exemple :

```C
int nombre = 0;
if (nombre == 0) {
    printf("Erreur : le nombre ne peut pas être égal à zéro\n");
}
```

Dans cet exemple, si la variable `nombre` est égale à zéro, le message "Erreur : le nombre ne peut pas être égal à zéro" sera imprimé à la console.

# Plongez plus profondément

Il existe de nombreuses façons d'utiliser la fonction `printf()` pour afficher des messages de débogage dans votre code. Par exemple, vous pouvez utiliser des opérateurs de formatage tels que `%f` pour les nombres à virgule flottante ou `%s` pour les chaînes de caractères.

Vous pouvez également utiliser des spécificateurs de largeur et de précision pour contrôler la mise en forme des données. Par exemple :

```C
float pourcentage = 50.123456;
printf("Le pourcentage est de %5.2f\n", pourcentage);
```

Cela affichera la sortie suivante :

```
Le pourcentage est de 50.12
```

Notez que `%5.2f` spécifie une précision de deux décimales et une largeur de cinq caractères pour le nombre à virgule flottante.

# Voir aussi

- [Guide complet de la fonction `printf()` en langage C] (lien vers un guide complet de la fonction `printf()` en français)
- [Comment utiliser la fonction `printf()` pour le débogage dans votre code C] (lien vers un article sur l'utilisation de la fonction `printf()` pour le débogage en anglais)