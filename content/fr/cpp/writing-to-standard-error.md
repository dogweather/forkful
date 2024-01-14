---
title:    "C++: Écrire vers l'erreur standard"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Dans les programmes C++, il est souvent nécessaire de signaler des erreurs ou des avertissements à l'utilisateur. Cela peut être fait à travers la sortie standard, mais il est également possible d'utiliser la sortie d'erreur standard (standard error) pour des messages plus importants ou critiques. Dans cet article, nous allons explorer pourquoi et comment écrire vers la sortie d'erreur standard en C++.

## Comment faire

Pour écrire vers la sortie d'erreur standard en C++, nous utilisons l'objet `cerr` qui est défini dans la bibliothèque standard `iostream`. Voici un exemple de code montrant comment écrire un message vers la sortie d'erreur standard :

```C++
#include <iostream>

int main() {
  std::cerr << "Ceci est un message d'erreur" << std::endl;
  return 0;
}
```

Dans cet exemple, nous utilisons l'opérateur de flux `<<` pour écrire le message vers la sortie d'erreur standard, suivi par `std::endl` pour ajouter un saut de ligne. Il est également possible d'utiliser `"\n"` pour un saut de ligne.

Il est important de noter que les messages écrits vers la sortie d'erreur standard ne sont pas mis en mémoire tampon, ce qui signifie qu'ils seront affichés immédiatement à l'écran, même si le programme se termine prématurément en raison d'une erreur.

Voici un exemple de sortie lorsque nous exécutons le programme :

```
Ceci est un message d'erreur
```

## Plongée profonde

En plus d'écrire simplement des messages d'erreur, il est également possible de définir des codes d'erreur personnalisés avec la fonction `exit()`. Cette fonction prend un code d'erreur entier en paramètre et arrête le programme, tout en retournant le code d'erreur à l'environnement d'exécution.

Voici un exemple de code montrant comment définir et retourner un code d'erreur :

```C++
#include <iostream>
#include <cstdlib>

int main() {
  int codeErreur = 5;
  std::cerr << "Une erreur s'est produite. Code d'erreur : " << codeErreur << std::endl;
  //terminer le programme avec le code d'erreur 5
  exit(codeErreur);
}
```

Il est également possible de rediriger la sortie d'erreur standard vers un fichier en utilisant `freopen()`. Cette fonction prend le nom du fichier et le mode d'ouverture en paramètres et redirige la sortie d'erreur vers ce fichier. Cela peut être utile pour enregistrer les messages d'erreur dans un fichier plutôt que de les afficher à l'écran.

## Voir aussi

- [Documentation Microsoft sur la sortie d'erreur standard en C++](https://docs.microsoft.com/fr-fr/cpp/standard-library/standard-error-output-cpp?view=msvc-160)
- [Article sur la redirection de la sortie d'erreur en C++](https://www.techiedelight.com/redirect-stderr-stdout-c/)
- [Article sur les codes d'erreur en C++](https://www.geeksforgeeks.org/introduction-user-defined-functions-c/)