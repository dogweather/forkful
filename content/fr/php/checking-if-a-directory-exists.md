---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:57:36.499021-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Quoi & Pourquoi ?
Vérifier l'existence d'un dossier permet de s'assurer qu'un chemin d’accès est valide avant d'effectuer des opérations de fichiers. Les programmeurs le font pour éviter des erreurs lors de la lecture, l'écriture ou la suppression de fichiers.

## How to - Comment faire :
```PHP
<?php
$dossier = "/chemin/vers/votre/dossier";

if (is_dir($dossier)) {
    echo "Le dossier existe.";
} else {
    echo "Le dossier n'existe pas.";
}
?>
```
Résultat :
```
Le dossier existe.
```
ou
```
Le dossier n'existe pas.
```
## Deep Dive - Exploration approfondie :
Historiquement, PHP offre plusieurs fonctions pour interagir avec le système de fichiers. La fonction `is_dir()` existe depuis les premières versions de PHP et est devenue un outil de base pour vérifier les dossiers. 

Comme alternative, la fonction `file_exists()` est aussi utilisée, mais attention : elle renvoie vrai pour les fichiers et les dossiers. 

Quant aux détails d'implémentation, `is_dir()` fonctionne non seulement avec des chemins relatifs et absolus, mais gère aussi les différences de plate-forme (Linux/Windows).

## See Also - Voir Aussi :
- Documentation officielle de `is_dir()`: https://www.php.net/manual/fr/function.is-dir.php
- Documentation officielle de `file_exists()`: https://www.php.net/manual/fr/function.file-exists.php
- Guide sur la manipulation des fichiers en PHP : https://www.php.net/manual/fr/book.filesystem.php
