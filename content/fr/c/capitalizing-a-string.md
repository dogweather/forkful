---
title:    "C: Capitaliser une chaîne de caractères"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation C, il peut être utile de capitaliser une chaîne de caractères pour des raisons esthétiques ou de manipulation de données. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en utilisant des outils de programmation C.

## Comment Faire

La première étape pour capitaliser une chaîne de caractères en C est de créer une fonction. Voici un exemple de code pour une telle fonction :

```C
void capitalize(char* str){
  // boucle à travers chaque caractère de la chaîne
  for(int i = 0; str[i] != '\0'; i++){
    // si le caractère est une lettre minuscule
    if(str[i] >= 'a' && str[i] <= 'z'){
      // convertir en majuscule
      str[i] = str[i] - 32;
    }
  }
}

int main(){
  char myString[] = "bonjour";
  // appel de la fonction pour capitaliser la chaîne
  capitalize(myString);
  // affichage du résultat
  printf("%s", myString); // affiche "BONJOUR"
  return 0;
}
```

Dans cet exemple, la fonction `capitalize` prend une chaîne de caractères en tant que paramètre et modifie la chaîne en convertissant toutes les lettres minuscules en majuscules. La boucle `for` parcourt chaque caractère de la chaîne et utilise une comparaison `if` pour vérifier si le caractère est une lettre minuscule, puis utilise une simple opération mathématique pour convertir le caractère en majuscule.

Il est important de noter que ce n'est qu'un exemple simplifié et qu'il existe de nombreuses façons de capitaliser une chaîne de caractères en C. Vous pouvez également utiliser des fonctions de la bibliothèque standard telles que `toupper()` ou même créer une fonction plus complexe pour gérer des cas spécifiques tels que la capitalisation des mots.

## Plongée en Profondeur

Il y a plusieurs choses à prendre en compte lors de la capitalisation d'une chaîne de caractères en C. Tout d'abord, il est important de comprendre la différence entre les caractères majuscules et minuscules en utilisant le système de codage ASCII. Ensuite, vous pouvez également envisager de gérer des caractères spéciaux ou des caractères avec des accents lors de la capitalisation.

De plus, il peut être utile d'ajouter des fonctionnalités supplémentaires à votre fonction de capitalisation, telles que la possibilité de capitaliser uniquement la première lettre d'un mot ou de traiter les abréviations qui doivent toujours être en majuscules.

## Voir Aussi

- [Documentation officielle sur les chaînes de caractères en C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Manipulation de chaînes de caractères en C](https://www.programiz.com/c-programming/c-strings)
- [Table ASCII complète](https://www.ionos.fr/digitalguide/sites-internet/developpement-web/code-ascii/)