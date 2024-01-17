---
title:                "Ecrire sur erreur standard"
html_title:           "PowerShell: Ecrire sur erreur standard"
simple_title:         "Ecrire sur erreur standard"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

L'écriture vers la sortie standard erreur (standard error) est un moyen de communiquer des messages d'erreur aux utilisateurs pendant l'exécution d'un script PowerShell. Les programmeurs l'utilisent pour diagnostiquer et résoudre des problèmes lors de l'exécution d'un script.

# Comment faire:

Pour écrire vers la sortie standard erreur en PowerShell, utilisez la commande `Write-Error` suivi du message d'erreur que vous souhaitez afficher. Voici un exemple de code:

```PowerShell
Write-Error "Erreur: le fichier spécifié n'est pas trouvé"
```

Lorsque vous exécutez le script, le message d'erreur sera affiché dans la console. Voici un exemple de sortie:

```
Erreur: le fichier spécifié n'est pas trouvé
```

Il est également possible d'utiliser la sortie standard erreur pour afficher des messages d'informations ou de mise en garde. Pour cela, utilisez la commande `Write-Error` suivi du paramètre `-InformationAction` ou `-WarningAction` avec la valeur `Continue`. Voici un exemple de code:

```PowerShell
Write-Error "Attention: le fichier doit être modifié" -WarningAction Continue
```

Lors de l'exécution du script, le message sera affiché avec une icône de mise en garde. Voici un exemple de sortie:

```
Attention: le fichier doit être modifié
```

# Plongée en profondeur:

L'écriture vers la sortie standard erreur existe depuis la première version de PowerShell et est largement utilisée par les programmeurs pour déboguer leurs scripts. Avant l'ajout de cette fonctionnalité, les erreurs étaient souvent masquées ou difficiles à trouver, rendant le processus de débogage plus long et fastidieux.

Une alternative à l'écriture vers la sortie standard erreur est d'utiliser la sortie standard (standard output) pour afficher des messages d'erreur. Cependant, cela peut causer des problèmes si la sortie standard est redirigée vers un fichier ou un autre programme.

Pour implémenter l'écriture vers la sortie standard erreur, PowerShell utilise le flux d'erreur (error stream) qui est séparé du flux de sortie standard. Cela permet aux messages d'erreur d'être gérés séparément par les utilisateurs.

# Voir aussi:

- [Documentation officielle de Microsoft sur la commande Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error)
- [Tutoriel sur l'écriture vers la sortie standard erreur en PowerShell](https://adamtheautomator.com/powershell-write-error/)