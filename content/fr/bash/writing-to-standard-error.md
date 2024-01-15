---
title:                "Écriture vers l'erreur standard"
html_title:           "Bash: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous avez peut-être déjà vu un message d'erreur s'afficher en rouge dans votre terminal lorsque vous exécutez un script Bash. Eh bien, c'était le résultat de l'écriture vers une sortie d'erreur standard. Dans cet article, nous allons explorer pourquoi et comment écrire vers la sortie d'erreur standard dans Bash.

## Comment faire
Pour écrire vers la sortie d'erreur standard, nous pouvons utiliser la commande `echo` en spécifiant le caractère spécial `>&2`. Voici un exemple:
```Bash
echo "Erreur: impossible d'ouvrir le fichier" >&2
```
Cela écrira le message d'erreur dans la sortie d'erreur standard plutôt que dans la sortie standard.

Il est également possible d'écrire vers la sortie d'erreur standard en utilisant l'opérateur de redirection `2>`. Voici un exemple:
```Bash
ls fichier_inexistant 2> erreurs.log
```
Cela redirigera toutes les erreurs provenant de la commande `ls` vers le fichier "erreurs.log".

## Plongée en profondeur
La sortie d'erreur standard est une des trois sorties standard dans un terminal Unix, les deux autres étant la sortie standard et l'entrée standard. La sortie d'erreur standard (ou stderr en anglais) est utilisée pour afficher les messages d'erreur et les avertissements lors de l'exécution d'un script Bash.

Écrire vers la sortie d'erreur standard peut être utile si vous souhaitez séparer les messages d'erreur de la sortie standard pour une meilleure lisibilité. De plus, certains programmes peuvent nécessiter la sortie d'erreur standard pour fonctionner correctement.

Il est également possible de rediriger la sortie d'erreur standard vers un fichier en utilisant l'opérateur `2>&1`. Cela redirigera à la fois la sortie d'erreur et la sortie standard vers le même fichier.

## Voir aussi
- [Document officiel de Bash](https://www.gnu.org/software/bash/)
- [Redirection de sortie standard et d'erreur standard](https://www.cyberciti.biz/faq/bash-redirecting-stderr-to-stdout/) (en anglais)
- [Tutoriel vidéo sur la redirection de la sortie d'erreur standard en Bash](https://www.youtube.com/watch?v=AnG_uF4-NAU) (en anglais)