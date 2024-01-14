---
title:                "Fish Shell: Écrire vers l'erreur standard"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire vers l'erreur standard en Fish Shell

Il peut être utile d'écrire vers l'erreur standard en programmation pour mieux comprendre les problèmes d'exécution de votre code. Cela peut également vous aider à déboguer plus efficacement en identifiant les erreurs plus rapidement.

## Comment faire

Voici un exemple de code utilisant la syntaxe Fish Shell pour écrire vers l'erreur standard :

```Fish Shell
function mon_script
    echo "Bonjour le monde !" >&2
end
```

Et voici le résultat que vous obtiendrez en exécutant cette fonction :

```
$ mon_script
Bonjour le monde !
```

Comme vous pouvez le voir, le message "Bonjour le monde !" est écrit vers l'erreur standard, symbolisée par le signe ">&2" à la fin de la commande "echo". Cela signifie que le message sera affiché en rouge dans votre terminal, ce qui le distingue des messages écrits vers la sortie standard.

## Approfondissement

Maintenant que vous savez comment écrire vers l'erreur standard en Fish Shell, voici quelques informations supplémentaires pour mieux comprendre cet outil :

- La commande ">&2" peut également être utilisée pour écrire des erreurs vers l'erreur standard dans des scripts Bash.
- Vous pouvez rediriger l'erreur standard vers un fichier en utilisant l'opérateur "2>" suivi du nom du fichier dans lequel vous souhaitez enregistrer les erreurs. Cela peut être utile pour créer des fichiers de journalisation ou de débogage.
- L'erreur standard est souvent utilisée pour afficher des messages d'erreur ou des avertissements à l'utilisateur.

# Voir aussi

- [La syntaxe Fish Shell complète](https://fishshell.com/docs/current/index.html)
- [Un guide détaillé sur les redirections en Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_redirects)