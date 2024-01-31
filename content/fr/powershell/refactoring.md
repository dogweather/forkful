---
title:                "Remaniement de code"
date:                  2024-01-26T03:37:28.774572-07:00
model:                 gpt-4-0125-preview
simple_title:         "Remaniement de code"

category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/refactoring.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La refactorisation est le processus de restructuration du code informatique existant sans en changer le comportement externe, dans le but d'améliorer les attributs non fonctionnels du logiciel. Les programmeurs refactorisent le code pour le rendre plus propre, plus efficace et plus facile à comprendre, ce qui facilite la maintenance ultérieure et les améliorations futures.

## Comment faire :
PowerShell ne dispose pas d'un outil de refactorisation dédié intégré, mais vous pouvez toujours nettoyer votre code pour améliorer la lisibilité et la performance. Considérez une fonction qui fait trop de choses et comment nous pourrions la refactoriser pour plus de clarté :

```PowerShell
function Get-InventoryData {
    # Fonction originale combinant la récupération et la mise en forme des données
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# Refactorisée en fonctions séparées
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# Utilisation
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

Sortie d'exemple :

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## Approfondissement
La refactorisation en programmation a des racines qui remontent aux premiers jours du développement logiciel, bien qu'elle ait été formalisée comme pratique dans les années 1990. Le livre de Martin Fowler "Refactoring: Improving the Design of Existing Code" est l'une des œuvres de référence sur le sujet, soulignant l'importance de la refactorisation pour obtenir un code propre.

Bien que PowerShell ne soit pas livré avec des outils de refactorisation spécifiques comme le font certains environnements de développement intégrés (IDE) pour d'autres langages (pensez à Eclipse ou Visual Studio), vous pouvez toujours pratiquer de bons principes de refactorisation manuellement. L'essentiel à retenir est que la refactorisation ne consiste pas simplement à changer le code pour le plaisir de le changer, mais à effectuer des modifications intentionnelles, préservant le comportement, qui améliorent la structure et la conception du code.

Les alternatives à la refactorisation manuelle dans PowerShell incluent l'utilisation d'IDE qui prennent en charge le langage, comme Visual Studio Code avec l'extension PowerShell, qui offre des fonctionnalités telles que la mise en forme du code et des capacités de refactorisation de base. Pour une refactorisation plus significative, vous pourriez envisager d'utiliser les tests Pester pour garantir que les modifications n'altèrent pas la fonctionnalité.

De plus, la mise en œuvre de la refactorisation peut impliquer des changements plus systémiques comme la modularisation, où le code est divisé en modules ou fonctions réutilisables, améliorant l'adhérence au principe DRY (Don't Repeat Yourself). D'autres techniques de refactorisation courantes incluent le renommage pour plus de clarté, la suppression du code en double et la réduction de la complexité de la logique conditionnelle.

## Voir aussi
Pour approfondir, voici quelques ressources :

- Le livre de refactorisation de Martin Fowler : [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- Tester le code refactorisé avec Pester : [Cadre de test Pester](https://pester.dev/)
- Meilleures pratiques PowerShell : [Le Guide des bonnes pratiques et du style PowerShell](https://poshcode.gitbooks.io/powershell-practice-and-style/)
