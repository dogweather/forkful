---
aliases:
- /fr/vba/using-an-interactive-shell-repl/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:42.747543-07:00
description: "Un shell interactif, ou boucle de lecture-\xE9valuation-affichage (REPL\
  \ pour Read-Eval-Print Loop), permet aux utilisateurs de saisir des commandes, de\
  \ les\u2026"
lastmod: 2024-02-18 23:09:08.580626
model: gpt-4-0125-preview
summary: "Un shell interactif, ou boucle de lecture-\xE9valuation-affichage (REPL\
  \ pour Read-Eval-Print Loop), permet aux utilisateurs de saisir des commandes, de\
  \ les\u2026"
title: Utiliser un shell interactif (REPL)
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Un shell interactif, ou boucle de lecture-évaluation-affichage (REPL pour Read-Eval-Print Loop), permet aux utilisateurs de saisir des commandes, de les exécuter et de voir les résultats en temps réel. Les programmeurs utilisent les REPL pour le prototypage rapide, le test de morceaux de code ou le débogage dans un environnement plus interactif et itératif, améliorant la productivité et la compréhension du code.

## Comment faire :

Visual Basic pour Applications (VBA) lui-même ne supporte pas nativement un shell interactif ou une expérience REPL comme on le voit dans des langages comme Python ou JavaScript. Cependant, vous pouvez simuler cette expérience dans une certaine mesure en utilisant la fenêtre Immédiate dans l'IDE VBA (Environnement de Développement Intégré).

**Accéder à la fenêtre Immédiate :**
1. Ouvrez l'IDE VBA en appuyant sur `Alt + F11` dans votre application Office.
2. Si la fenêtre Immédiate n'est pas visible, vous pouvez l'ouvrir en appuyant sur `Ctrl + G` ou en la sélectionnant à partir du menu Affichage.

**Utiliser la fenêtre Immédiate comme un REPL :**
- Pour exécuter une ligne de code, tapez-la simplement dans la fenêtre Immédiate et appuyez sur Entrée. Par exemple :

```basic
Debug.Print 2 + 2
```

- Sortie d'exemple :
```
 4
```

- Vous pouvez également appeler des fonctions et des sous-routines définies dans vos modules :

```basic
Public Sub SayHello()
    Debug.Print "Bonjour, le monde !"
End Sub
```

- Et puis dans la fenêtre Immédiate :
```basic
Call SayHello
```

- Sortie d'exemple :
```
 Bonjour, le monde !
```

**Note :** La fenêtre Immédiate a des limitations. Elle est excellente pour des tests rapides et des appels de fonction directs, mais elle ne supporte pas la définition de fonctions ou de sous-routines directement en son sein. Les tâches de débogage et de programmation complexes pourraient nécessiter un développement de module complet.

## Exploration approfondie

La fenêtre Immédiate dans VBA sert de contrepartie la plus proche aux shells interactifs trouvés dans d'autres écosystèmes de programmation, malgré ses limitations. Historiquement, VBA a été axé sur l'extension des capacités des applications Microsoft Office par le biais de scripts et de macros plutôt que sur le développement de logiciels autonomes, ce qui pourrait expliquer l'absence d'un REPL à part entière.

Pour les tâches nécessitant des tests interactifs étendus ou le développement de logiques complexes, d'autres environnements de programmation équipés d'un support REPL natif, tels que Python avec son IDLE, ou JavaScript avec Node.js, pourraient offrir de meilleures alternatives. Ces environnements fournissent non seulement des shells interactifs, mais aussi des installations de programmation, de débogage et de test plus robustes.

La fenêtre Immédiate offre néanmoins un outil inestimable pour tester rapidement des expressions, exécuter des fonctions et manipuler directement les objets des applications Office. Ainsi, elle occupe une niche vitale dans le processus de développement VBA, offrant une immédiateté et une commodité inégalées par des cycles de compilation-exécution-débogage plus traditionnels, bien que dans les limites comprises de son champ opérationnel.
