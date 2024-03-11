---
date: 2024-01-26 00:56:05.110935-07:00
description: "G\xE9rer les erreurs avec PowerShell signifie anticiper les incidents\
  \ et les g\xE9rer en douceur. Les programmeurs font cela pour \xE9viter les plantages\
  \ et\u2026"
lastmod: '2024-03-11T00:14:31.978547-06:00'
model: gpt-4-1106-preview
summary: "G\xE9rer les erreurs avec PowerShell signifie anticiper les incidents et\
  \ les g\xE9rer en douceur. Les programmeurs font cela pour \xE9viter les plantages\
  \ et\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Gérer les erreurs avec PowerShell signifie anticiper les incidents et les gérer en douceur. Les programmeurs font cela pour éviter les plantages et fournir un retour utile aux utilisateurs.

## Comment :
```PowerShell
# Try-Catch basique pour gérer les exceptions
try {
    # Code qui pourrait déclencher une erreur
    $resultat = 1 / 0
} catch {
    # Que faire si une erreur survient
    Write-Host "Oups, une erreur est survenue : $_"
}

# Affichage d'un message d'erreur personnalisé
try {
    Get-Item "fichiernonexistant.txt" -ErrorAction Stop
} catch {
    Write-Host "Le fichier n'a pas pu être trouvé."
}

# Utilisation de la variable $Error pour inspecter la dernière erreur
```
## En profondeur
PowerShell a considérablement évolué depuis ses débuts sous le nom de Monad. La gestion des erreurs est devenue plus robuste au fil du temps, offrant des fonctionnalités similaires à d'autres langages de programmation. La syntaxe `try-catch-finally` est l'une de ces influences croisées de langages comme C#. Avant, les scripteurs se reposaient fortement sur la vérification des conditions et l'utilisation de la variable automatique `$Error`.

PowerShell distingue également deux principaux types d'erreurs : terminantes et non-terminantes. Les erreurs terminantes arrêteront le script à moins d'être capturées dans un bloc `try-catch`, tandis que les non-terminantes ne le feront pas, à moins que vous ne spécifiiez `-ErrorAction Stop`. Cette distinction est cruciale car elle offre un contrôle précis sur la gestion des erreurs, décidant si une erreur justifie vraiment l'arrêt de tout le script ou si elle peut simplement être consignée et ignorée.

La gestion des erreurs de PowerShell permet également d'utiliser un bloc `finally`, qui s'exécute quoi qu'il arrive - qu'une erreur se soit produite ou non. Il est idéal pour les tâches de nettoyage.

Lorsque vous êtes profondément dans les tranchées du script, vous pouvez également gérer des types d'exceptions spécifiques, ce qui vous donne un contrôle encore plus fin.

Alternativement, il y a l'ancienne méthode du paramètre `-ErrorVariable` pour capturer les erreurs sans lever d'exception. Et la variable `$?` vous indique si la dernière opération a réussi. Ce sont des outils pratiques, bien qu'un peu moins clairs qu'un solide `try-catch`.

## Voir aussi
- [about_Try_Catch_Finally](https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
