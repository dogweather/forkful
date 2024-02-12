---
title:                "Obtenir la date actuelle"
aliases:
- /fr/google-apps-script/getting-the-current-date.md
date:                  2024-02-01T21:54:30.976121-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/google-apps-script/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Obtenir la date actuelle avec Google Apps Script consiste à récupérer la date et l'heure en direct, une tâche courante pour l'automatisation des tâches, la journalisation et l'horodatage dans les applications liées à l'écosystème de Google. Les programmeurs utilisent cela pour la génération de contenu dynamique, le suivi des échéances et la planification dans Google Docs, Sheets et d'autres services Google.

## Comment :

Google Apps Script, qui est basé sur JavaScript, offre des méthodes simples pour obtenir la date actuelle. Vous pouvez utiliser le constructeur `new Date()` pour créer un nouvel objet date représentant la date et l'heure actuelles. Voici comment vous pouvez manipuler et afficher cela dans divers formats.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Journalise la date et l'heure actuelles dans le fuseau horaire du script
  
  // Pour afficher juste la date au format AAAA-MM-JJ
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Exemple de sortie: "2023-04-01"
  
  // Affichage dans un format plus lisible
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Exemple de sortie: "1 avril 2023, 12:00:00 PM GMT+1"
}
```

Ces extraits démontrent comment capturer et formater la date et l'heure actuelles, montrant la polyvalence pour divers besoins de programmation au sein de Google Apps Script.

## Plongée Profonde

Avant que JavaScript ne se stabilise sur l'objet `Date`, les programmeurs devaient manuellement garder une trace du temps et de la date à travers des moyens moins standards et plus encombrants. Cela incluait l'utilisation d'entiers timestamp et de fonctions de date faites maison, qui variaient d'un environnement de programmation à l'autre, conduisant à des incohérences et des problèmes de compatibilité.

L'introduction de l'objet `new Date()` dans JavaScript, et par extension dans Google Apps Script, a standardisé les opérations de date et d'heure, les rendant plus intuitives et réduisant la quantité de code nécessaire pour les opérations liées à la date. Il convient de noter que, bien que l'implémentation de Google Apps Script soit pratique et suffisante pour de nombreuses applications au sein de la gamme de produits de Google, elle peut ne pas répondre à tous les scénarios, surtout ceux nécessitant une gestion complexe des fuseaux horaires ou une journalisation précise des timestamps dans des environnements rapides.

Pour de tels cas d'utilisation avancés, les programmeurs se tournent souvent vers des bibliothèques telles que Moment.js ou date-fns en JavaScript. Bien que Google Apps Script ne prenne pas en charge nativement ces bibliothèques, les développeurs peuvent imiter certaines de leurs fonctionnalités en utilisant les méthodes Date JavaScript disponibles ou en accédant à des bibliothèques externes via le service HTML ou le service de récupération d'URL d'Apps Script. Malgré ces alternatives, la simplicité et l'intégration des fonctions de date et d'heure natives de Google Apps Script restent un choix privilégié pour la plupart des tâches dans l'écosystème Google.
