---
title:    "Bash: Écriture vers l'erreur standard"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi écrire vers la sortie d'erreur ?

Si vous êtes un programmeur Bash, vous avez probablement déjà rencontré des erreurs inattendues dans votre code. Les messages d'erreur sont essentiels pour déboguer votre programme et comprendre où il a mal fonctionné. Mais saviez-vous qu'il est possible d'écrire spécifiquement vers la sortie d'erreur plutôt que la sortie standard ? Cela peut s'avérer très utile pour organiser et hiérarchiser les différentes sorties de votre programme.

## Comment le faire ?

Pour écrire vers la sortie d'erreur, vous devez utiliser la commande `>&2` dans votre code. Cela va rediriger la sortie vers le flux d'erreur au lieu du flux standard. Par exemple, si vous voulez afficher un message d'erreur lorsqu'un fichier n'est pas trouvé, vous pouvez utiliser la syntaxe suivante :

```Bash
if [ ! -f fichier.txt ]; then
  echo "Erreur : Le fichier fichier.txt est introuvable" >&2
fi
```

Cette commande affichera le message d'erreur dans la sortie d'erreur, ce qui permettra de mieux identifier et traiter l'erreur.

## Plongez dans les détails

L'avantage de rediriger vers la sortie d'erreur est qu'elle peut être utilisée de différentes manières selon vos besoins. Par exemple, vous pouvez également utiliser la syntaxe `2>&1` pour rediriger la sortie standard vers la sortie d'erreur. Vous pouvez également utiliser `2>/dev/null` pour supprimer complètement la sortie d'erreur, ce qui peut s'avérer utile si vous ne voulez pas afficher de messages inutiles.

Il est également important de noter que lors de l'utilisation de la commande `>&2`, vous pouvez rediriger différents types de sorties vers la sortie d'erreur, tels que les messages de débogage, les avertissements ou les erreurs.

## Voir aussi

- [La documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html#Redirections)
- [Un tutoriel détaillé sur la redirection en Bash](https://www.cyberciti.biz/faq/how-to-redirect-output-and-errors-to-devnull-on-bash-linux/)