---
title:                "Modification de fichiers sur place avec des lignes de commande en une seule étape"
date:                  2024-01-27T16:20:35.926975-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modification de fichiers sur place avec des lignes de commande en une seule étape"

category:             "PowerShell"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/editing-files-in-place-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

L'édition de fichiers directement avec des lignes de commande (CLI) en une ligne avec PowerShell consiste à effectuer des modifications directes sur les fichiers depuis la ligne de commande, sans avoir besoin de les ouvrir dans un éditeur. Cette approche permet de gagner du temps et peut être particulièrement utile pour le traitement par lots ou l’automatisation des tâches d’édition répétitives sur plusieurs fichiers.

## Comment faire :

### Remplacer du texte dans un fichier unique

Commençons par une tâche simple : vous voulez remplacer toutes les instances de "oldtext" par "newtext" dans un fichier nommé example.txt. Voici comment vous pouvez le faire :

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Cette ligne de commande lit le contenu, effectue le remplacement et réécrit le contenu dans le fichier original.

### Éditer plusieurs fichiers

Que faire si vous avez besoin d'appliquer le même changement à plusieurs fichiers ? Voici une approche utilisant une boucle :

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Ce bout de code trouve tous les fichiers `.txt` dans le répertoire courant, en remplaçant "oldtext" par "newtext" dans chacun d'eux.

### Ajouter du contenu au début ou à la fin des fichiers

Ajouter ou préfixer du contenu peut également être simplifié :

```PowerShell
# Préfixer
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Ajouter à la fin
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

Ici, nous concaténons simplement le nouveau contenu avant ou après le contenu existant et le sauvegardons.

## Plongée profonde

Historiquement, l'édition directe dans les fichiers est plus généralement associée à des outils Unix tels que `sed` et `awk`. PowerShell, étant un nouvel arrivant, n'inclut pas de fonctionnalité d'édition directe en soi. Cela est en partie dû à sa philosophie de conception, soulignant l'importance des objets par rapport aux flux de texte, contrairement aux outils Unix qui traitent la plupart des entrées comme du texte.

Les alternatives à PowerShell pour cette tâche incluent l'utilisation d'outils Unix traditionnels disponibles sur Windows via Cygwin ou le sous-système Windows pour Linux (WSL). Ces outils offrent souvent une syntaxe plus concise pour l'édition directe en raison de leur conception centrée sur le texte.

Concernant la mise en œuvre, il est important de noter que l'approche de PowerShell implique de lire le fichier entier en mémoire, de faire des modifications, puis de l'écrire à nouveau. Bien que cela fonctionne bien pour des fichiers de taille modérée, cela peut devenir inefficace pour des fichiers très volumineux. Dans de tels cas, on pourrait envisager d'utiliser directement les méthodes `.NET` ou de recourir à d'autres outils conçus pour le streaming de grandes quantités de données.

Malgré ces considérations, la flexibilité de PowerShell et son ensemble de fonctionnalités étendues en font un outil inestimable pour manipuler des fichiers directement depuis la ligne de commande, en particulier pour ceux déjà ancrés dans l'écosystème Windows ou gérant des environnements multiplateformes.
