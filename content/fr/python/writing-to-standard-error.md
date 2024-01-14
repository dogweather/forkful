---
title:                "Python: Écrire sur le flux d'erreur standard"
simple_title:         "Écrire sur le flux d'erreur standard"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi

L'écriture sur l'erreur standard, ou stderr en anglais, peut sembler intimidante pour certains programmeurs débutants. Cependant, c'est une compétence essentielle à maîtriser si vous voulez devenir un développeur Python accompli. En écrivant sur l'erreur standard, vous pouvez facilement signaler et gérer les erreurs ou les exceptions dans votre code, ce qui rendra vos programmes plus robustes et plus fiables.

# Comment faire

Pour écrire sur l'erreur standard en Python, vous pouvez utiliser la fonction "print" avec le paramètre "file=sys.stderr". Cela enverra le texte passé en argument sur l'erreur standard plutôt que sur la sortie standard.

Un exemple simple de code utilisant cette méthode pourrait ressembler à ceci :

```
import sys

print("Je vais être écrit sur l'erreur standard", file=sys.stderr)
```

La sortie de ce code serait :

```
Je vais être écrit sur l'erreur standard
```

Remarque : Il est important de noter que si votre code n'a aucun bug ou exception, rien ne sera imprimé sur l'erreur standard.

# Profonde plongée

Maintenant que vous savez comment écrire sur l'erreur standard en Python, il est intéressant de comprendre comment cela fonctionne en interne. En fait, la fonction "print" avec le paramètre "file" n'écrit pas directement sur l'erreur standard, mais sur un objet "file" Python qui correspond à l'erreur standard. Cela signifie que vous pouvez également utiliser des opérations de fichiers telles que "write" ou "writelines" pour écrire du contenu sur l'erreur standard.

De plus, contrairement à l'erreur standard en C, l'erreur standard en Python est un objet dynamique pouvant être redéfini par l'utilisateur. Cela signifie que vous pouvez changer l'objet "sys.stderr" pour diriger l'erreur standard vers un fichier spécifique ou même vers un journal personnalisé.

# Voir aussi

Voici quelques liens utiles pour en savoir plus sur l'écriture sur l'erreur standard en Python :

* [La documentation officielle de Python sur "print"](https://docs.python.org/fr/3/library/functions.html#print)
* [Un tutoriel sur la redirection de l'erreur standard avec "sys.stderr"](https://www.geeksforgeeks.org/redirecting-python-stderr-and-stdout-to-a-log-file/)
* [Un article sur la personnalisation de l'erreur standard en Python](https://www.codementor.io/@arpitbhayani/personalize-your-python-exception-using-sys-exc_info-stack-and-friends-aibdjrnx6)