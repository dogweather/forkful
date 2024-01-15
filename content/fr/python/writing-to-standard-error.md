---
title:                "Écrire sur la sortie standard"
html_title:           "Python: Écrire sur la sortie standard"
simple_title:         "Écrire sur la sortie standard"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire dans la sortie d'erreur standard (standard error) peut sembler effrayant pour les débutants en programmation, mais c'est en fait une compétence importante à maîtriser. Cela peut vous aider à comprendre les erreurs dans votre code et à les résoudre plus facilement, ce qui vous permettra de devenir un meilleur programmeur.

## Comment

Pour écrire dans la sortie d'erreur standard en Python, vous pouvez utiliser la fonction intégrée "stderr" du module "sys". Elle accepte une chaîne de caractères comme argument et l'affichera dans la sortie d'erreur standard. Voici un exemple de code :

```Python
import sys
sys.stderr.write("Une erreur s'est produite !")
```

La sortie sera alors :

```
Une erreur s'est produite !
```

Cette fonction peut être utile lorsque vous voulez afficher un message d'erreur spécifique dans votre code, au lieu de simplement laisser le programme planter.

## Plongée en profondeur

La sortie d'erreur standard en Python est également utile pour déboguer votre code. Lorsqu'une erreur se produit, vous pouvez utiliser la fonction "print" pour afficher des messages dans la sortie standard, mais cela peut interférer avec le déroulement normal de votre programme. En écrivant dans la sortie d'erreur standard, vous pouvez séparer les messages d'erreur du reste de votre code et les afficher sans perturber l'exécution du programme.

De plus, en utilisant la redirection de la sortie d'erreur standard vers un fichier, vous pouvez enregistrer les erreurs dans un fichier pour les examiner plus tard et les corriger. Cela peut être particulièrement utile lorsque votre programme est exécuté sur un serveur distant sans accès à la sortie standard en direct.

## Voir aussi

- Documentation officielle de Python sur la fonction "stderr" : https://docs.python.org/fr/3/library/sys.html#sys.stderr
- Tutoriel sur les erreurs et exceptions en Python : https://www.programiz.com/python-programming/exceptions
- Plus d'informations sur la redirection de la sortie d'erreur standard en utilisant la ligne de commande : https://www.geeksforgeeks.org/redirecting-stderr-stdout-stderr/