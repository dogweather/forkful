---
title:                "Utilisation d'un débogueur"
aliases:
- /fr/javascript/using-a-debugger/
date:                  2024-01-26T03:49:46.740623-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'un débogueur"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/using-a-debugger.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Utiliser un débogueur signifie recourir à des outils spécialisés qui vous permettent de jeter un coup d'œil sous le capot de votre code, en le regardant s'exécuter étape par étape. Les programmeurs font cela pour écraser les bugs, optimiser les performances et comprendre le comportement du code.

## Comment faire :
Voici un peu de code JavaScript qui ne se comporte pas comme prévu :

```javascript
function buggyMultiply(a, b) {
    return a + b; // Oups ! Cela devrait être une multiplication, pas une addition.
}

let result = buggyMultiply(5, 3);
console.log('Résultat :', result);
```

Le résultat est incorrect :
```
Résultat : 8
```

Débuguons dans Chrome DevTools :

1. Ouvrez ce JS dans un navigateur.
2. Faites un clic droit et sélectionnez "Inspecter" pour ouvrir DevTools.
3. Cliquez sur l'onglet "Sources".
4. Trouvez votre extrait de code ou page et placez un point d'arrêt en cliquant sur le numéro de ligne à côté de l'instruction `return`.
5. Actualisez la page pour déclencher le point d'arrêt.
6. Consultez le panneau "Scope" pour voir les variables locales `a` et `b`.
7. Avancez avec le bouton "Passer à l'appel de fonction suivant".
8. Repérez le bug dans l'instruction `return`.
9. Corrigez le code :
```javascript
function buggyMultiply(a, b) {
    return a * b; // Corrigé !
}

let result = buggyMultiply(5, 3);
console.log('Résultat :', result);
```

Le résultat corrigé :
```
Résultat : 15
```

## Plongée profonde
Le concept de débogage existe depuis les premiers jours de l'informatique—la légende dit qu'il a commencé lorsqu'une mite a été trouvée dans un ordinateur dans les années 1940 ! Aujourd'hui, les débogueurs JavaScript comme les outils intégrés aux navigateurs (Chrome DevTools, Firefox Developer Tools) ou les débogueurs intégrés aux IDE (Visual Studio Code, WebStorm) offrent une tonne de fonctionnalités.

Les alternatives aux débogueurs intégrés incluent des outils tiers comme WebStorm ou l'utilisation du bon vieux `console.log` pour sortir les états des variables. Mais ceux-ci n'offrent pas l'interaction en temps réel et l'inspection détaillée fournies par les débogueurs.

En ce qui concerne les détails d'implémentation, la plupart des débogueurs fonctionnent de manière similaire : ils vous permettent de définir des points d'arrêt qui interrompent l'exécution, de passer à travers le code, d'inspecter les états actuels des variables, de surveiller des expressions et même de manipuler des valeurs à la volée pour tester différents scénarios.

## Voir aussi
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Débogueur Firefox](https://developer.mozilla.org/fr/docs/Outils/Débogueur)
- [Visual Studio Code - Débogage](https://code.visualstudio.com/docs/editor/debugging)
