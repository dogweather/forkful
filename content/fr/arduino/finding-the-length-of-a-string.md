---
title:                "Arduino: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un passionné de l'Arduino, vous savez que la programmation joue un rôle crucial dans la création de projets intéressants et fonctionnels. Une des choses les plus courantes que vous aurez à faire en programmation est de trouver la longueur d'une chaîne de caractères. Bien que cela puisse sembler simple au premier abord, c'est une compétence importante à maîtriser car elle peut être utilisée dans une variété de projets et de situations.

## Comment faire 

Pour trouver la longueur d'une chaîne de caractères en utilisant l'Arduino, vous devrez utiliser une fonction appelée ```length```. Cette fonction prend une chaîne de caractères en entrée et renvoie le nombre de caractères qu'elle contient.

Pour illustrer cela, voici un exemple de code qui utilise la fonction ```length``` pour trouver la longueur de la chaîne "Bonjour Arduino !" :

```Arduino
String chaine = "Bonjour Arduino !";
Serial.println(chaine.length());
```

Dans cet exemple, la fonction ```length``` est utilisée pour renvoyer le nombre de caractères présents dans la chaîne "Bonjour Arduino !". Lorsque vous téléversez ce code sur votre carte Arduino, vous verrez le résultat s'afficher dans le moniteur série.

## Plongée en profondeur

Maintenant que vous savez comment utiliser la fonction ```length```, voyons comment cela fonctionne en interne. En réalité, la fonction ```length``` est une méthode de la classe ```String```. Cela signifie qu'elle peut être utilisée pour n'importe quelle chaîne de caractères créée à l'aide de la classe ```String```.

Lorsque vous utilisez la fonction ```length```, elle parcourt la chaîne de caractères et compte le nombre de caractères avant de le renvoyer en tant que résultat. Cela signifie que la fonction doit parcourir toute la chaîne, ce qui peut prendre plus de temps si la chaîne est très longue. C'est pourquoi il est important d'utiliser cette fonction avec parcimonie dans votre code.

## Voir aussi 

Pour en savoir plus sur la fonction ```length``` et sur d'autres méthodes utiles de la classe ```String```, vous pouvez consulter les liens suivants :

- [Documentation officielle d'Arduino pour la classe String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Un tutoriel en français sur la manipulation de chaînes de caractères avec l'Arduino](https://wiki.mchobby.be/index.php?title=Prog-Arduino-Strings&oldid=16043)
- [Un forum de discussion sur les chaînes de caractères en programmation avec l'Arduino](https://forum.arduino.cc/index.php?topic=174348.0)

En utilisant la fonction ```length``` et en comprenant son fonctionnement en profondeur, vous serez mieux équipé pour créer des projets passionnants avec votre Arduino. N'hésitez pas à explorer d'autres fonctions et techniques de programmation pour améliorer vos compétences et créer des projets encore plus intéressants. Bonne chance !