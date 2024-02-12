---
title:                "Gestion des erreurs"
aliases:
- /fr/google-apps-script/handling-errors.md
date:                  2024-02-01T21:54:52.784475-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gestion des erreurs"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/handling-errors.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La gestion des erreurs dans Google Apps Script consiste à prévoir, attraper et répondre aux exceptions ou erreurs qui surviennent pendant l'exécution du script. Les programmeurs la mettent en œuvre pour protéger les scripts contre les défaillances inattendues, garantissant des applications plus fluides et conviviales qui peuvent gérer ou enregistrer les erreurs avec grâce sans plantages brusques.

## Comment faire :

Google Apps Script, étant basé sur JavaScript, nous permet d'utiliser l'instruction traditionnelle `try-catch` pour la gestion des erreurs, accompagnée de `finally` si un nettoyage est nécessaire quel que soit le succès ou l'erreur.

```javascript
function myFunction() {
  try {
    // Code susceptible de générer une erreur
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("La cellule A1 est vide.");
    }
    Logger.log(data);
  } catch (e) {
    // Code de gestion des erreurs
    Logger.log("Erreur : " + e.message);
  } finally {
    // Code de nettoyage, exécuté qu'une erreur se soit produite ou non
    Logger.log("Fonction terminée.");
  }
}
```

Exemple de sortie sans erreur :
```
[Valeur de la cellule]
Fonction terminée.
```

Exemple de sortie avec une erreur (en supposant que A1 est vide) :
```
Erreur : La cellule A1 est vide.
Fonction terminée.
```

Google Apps Script prend également en charge le lancement d'erreurs personnalisées à l'aide de l'objet `Error` et la capture de types d'erreurs spécifiques si nécessaire. Cependant, l'absence de catégorisation avancée des erreurs rend essentiel de se fier aux messages d'erreur pour la spécificité.

## Plongée profonde

Historiquement, la gestion des erreurs dans les langages de script comme JavaScript (et par extension, Google Apps Script) a été moins sophistiquée que dans certains langages compilés, qui offrent des fonctionnalités telles que des hiérarchies d'exceptions détaillées et des outils de débogage complets. Le modèle de Google Apps Script est relativement simple, exploitant le paradigme `try-catch-finally` de JavaScript. Cette simplicité s'aligne sur la conception du langage pour développer et déployer rapidement des applications à petite et moyenne échelle au sein de l'écosystème de Google, mais cela peut parfois limiter les développeurs confrontés à des scénarios d'erreur complexes.

Dans des applications plus complexes, les programmeurs complètent souvent la gestion des erreurs native de Google Apps Script avec des mécanismes de journalisation et de rapport d'erreurs personnalisés. Cela peut inclure l'écriture d'erreurs dans une feuille Google pour l'audit ou l'utilisation de services de journalisation tiers par le biais des Services de récupération d'URL de Google Apps Script pour envoyer les détails des erreurs hors de l'environnement du script.

Bien que Google Apps Script puisse être en retard sur des langages comme Java ou C# en termes de complexité et de capacités de gestion des erreurs intégrées, son intégration avec les services Google et la simplicité de l'approche `try-catch-finally` en font un outil puissant pour les développeurs pour automatiser rapidement des tâches et créer des intégrations au sein de l'écosystème Google. Les développeurs venant d'autres horizons peuvent trouver que le défi ne réside pas dans la maîtrise de modèles de gestion des erreurs complexes, mais dans l'exploitation créative de ce qui est disponible pour garantir que leurs scripts sont robustes et conviviaux.
