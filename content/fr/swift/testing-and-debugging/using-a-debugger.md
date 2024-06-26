---
date: 2024-01-26 04:10:41.806608-07:00
description: "Comment faire : Pour utiliser le d\xE9bogueur dans Xcode (l'IDE pour\
  \ Swift), vous pouvez d\xE9finir des points d'arr\xEAt, inspecter des variables\
  \ et observer des\u2026"
lastmod: '2024-03-13T22:44:58.227520-06:00'
model: gpt-4-0125-preview
summary: "Pour utiliser le d\xE9bogueur dans Xcode (l'IDE pour Swift), vous pouvez\
  \ d\xE9finir des points d'arr\xEAt, inspecter des variables et observer des expressions."
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Pour utiliser le débogueur dans Xcode (l'IDE pour Swift), vous pouvez définir des points d'arrêt, inspecter des variables et observer des expressions. Voici un exemple :

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Définissez un point d'arrêt en cliquant à gauche d'un numéro de ligne dans Xcode, et exécutez le programme. Lorsqu'il atteint le point d'arrêt, Xcode interrompt l'exécution. Vous pouvez maintenant :

1. Vérifier les valeurs des variables.
2. Passer à l'étape suivante (exécuter la ligne suivante) ou entrer dans l'étape (aller à l'intérieur d'une fonction) en utilisant les contrôles du débogueur.
3. Ajouter des expressions à la 'liste de surveillance' pour surveiller les changements dans des variables ou constantes spécifiques.

Voici ce que vous pourriez voir dans la zone de débogage :

```
(lldb) po number
5
(lldb) po result
120
```

## Plongée Profonde :
Les débogueurs font partie du paysage de la programmation depuis les années 1940, évoluant de systèmes de points d'arrêt simples à des expériences complexes pilotées par l'interface utilisateur. Outre le débogueur intégré à Xcode, d'autres options incluent des outils tiers comme LLDB (Low Level Debugger), que Xcode utilise sous le capot. Certains déboguent même avec des déclarations `print()` (affectueusement connues sous le nom de "débogage des hommes des cavernes"), mais cela s'avère moins efficace pour les grands projets ou les bugs complexes. Lorsque vous utilisez un débogueur, vous jonglez avec le contrôle de l'exécution, l'introspection à l'exécution, et la manipulation des données. Une compréhension approfondie de ces principes contribue grandement à un débogage efficace.

## Voir Aussi :
- [Guide de débogage Xcode d'Apple](https://developer.apple.com/documentation/xcode/debugging/)
- [Guide de démarrage rapide LLDB](https://lldb.llvm.org/use/tutorial.html)
- [Tutoriel de débogage Swift de Ray Wenderlich](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
