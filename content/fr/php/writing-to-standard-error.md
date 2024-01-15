---
title:                "Écrire sur l'erreur standard"
html_title:           "PHP: Écrire sur l'erreur standard"
simple_title:         "Écrire sur l'erreur standard"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi

Ecrire sur le flux d'erreur (standard error) est un moyen simple et efficace d'enregistrer des informations à des fins de débogage ou de suivi des erreurs lors du développement d'applications en PHP.

# Comment Faire

L'écriture sur le flux d'erreur en PHP est très simple. Tout d'abord, il est important de comprendre que le flux d'erreur est un flux de sortie par lequel les erreurs et les avertissements sont généralement affichés. Pour y écrire, il suffit d'utiliser la fonction "fwrite()" en spécifiant le flux dans lequel écrire, suivi du message à enregistrer.

Par exemple :

```PHP
fwrite(STDERR, "Erreur : Impossible de se connecter à la base de données !");
```

Ce code écrira le message "Erreur : Impossible de se connecter à la base de données !" sur le flux d'erreur.

Si vous voulez enregistrer une erreur spécifique à une variable, vous pouvez utiliser la fonction "error_log()" pour écrire directement sur le flux d'erreur. Par exemple :

```PHP
$erreur = "Erreur : Impossible de se connecter à la base de données !";
error_log($erreur, 3, "/chemin/vers/le/fichier/erreur.log");
```

Ce code écrira le message dans le fichier "erreur.log" situé dans le chemin spécifié.

# Plongée Profonde

Ecrire sur le flux d'erreur peut également être très utile lors du débogage d'une application. En utilisant la fonction "error_reporting()", vous pouvez définir le niveau des erreurs à afficher sur le flux d'erreur, ce qui vous permet de ne voir que les erreurs pertinentes pour votre débogage. Par exemple :

```PHP
error_reporting(E_ERROR | E_WARNING | E_PARSE);
```

Ce code définira le niveau d'erreur à afficher sur le flux d'erreur pour inclure uniquement les erreurs fatales, les avertissements et les erreurs de syntaxe.

Il est également possible d'activer l'écriture sur le flux d'erreur dans le fichier de configuration php.ini en définissant l'option "log_errors" sur "On". Cela enregistrera toutes les erreurs sur le flux d'erreur.

# Voir Aussi
- [Documentation officielle de PHP sur les flux de sortie](https://www.php.net/manual/fr/features.commandline.io-streams.php)
- [Guide complet sur la gestion des erreurs en PHP](https://www.php.net/manual/fr/book.errorfunc.php)