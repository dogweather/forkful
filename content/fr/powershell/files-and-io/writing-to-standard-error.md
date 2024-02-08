---
title:                "Écrire sur l'erreur standard"
date:                  2024-02-03T19:34:08.625383-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard (stderr) dans PowerShell implique d'envoyer des messages d'erreur ou des diagnostics directement sur le flux stderr, distinct du flux de sortie standard (stdout). Cette séparation permet un contrôle plus précis sur la sortie d'un script, permettant aux développeurs de diriger les messages normaux et les messages d'erreur vers des destinations différentes, ce qui est fondamental pour la gestion des erreurs et la journalisation.

## Comment faire :

PowerShell simplifie le processus d'écriture sur stderr grâce à l'utilisation du cmdlet `Write-Error` ou en dirigeant la sortie vers la méthode `$host.ui.WriteErrorLine()`. Cependant, pour une redirection directe de stderr, vous pourriez préférer utiliser les méthodes .NET ou la redirection des descripteurs de fichiers proposée par PowerShell lui-même.

**Exemple 1 :** Utilisation de `Write-Error` pour écrire un message d'erreur sur stderr.

```powershell
Write-Error "Ceci est un message d'erreur."
```

Sortie sur stderr :
```
Write-Error: Ceci est un message d'erreur.
```

**Exemple 2 :** Utilisation de `$host.ui.WriteErrorLine()` pour une écriture directe sur stderr.

```powershell
$host.ui.WriteErrorLine("Écriture directe sur stderr.")
```

Sortie sur stderr :
```
Écriture directe sur stderr.
```

**Exemple 3 :** Utilisation des méthodes .NET pour écrire sur stderr.

```powershell
[Console]::Error.WriteLine("Utilisation de méthode .NET pour stderr")
```

Sortie de cette méthode :
```
Utilisation de méthode .NET pour stderr
```

**Exemple 4 :** Redirection de la sortie d'erreur en utilisant le descripteur de fichier `2>`.

Les descripteurs de fichier dans PowerShell peuvent rediriger différents flux. Pour stderr, le descripteur de fichier est `2`. Voici un exemple de redirection de stderr vers un fichier nommé `error.log` lors de l'exécution d'une commande qui génère une erreur.

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

Cet exemple ne produit pas de sortie console, mais génère un fichier `error.log` dans le répertoire courant contenant le message d'erreur résultant de la tentative d'accès à un fichier qui n'existe pas.

En conclusion, PowerShell offre plusieurs méthodes pour écrire et gérer efficacement la sortie d'erreur, permettant des stratégies sophistiquées de gestion des erreurs et de journalisation dans des scripts et applications.
