---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:20.065001-07:00
description: "Comment : Dans Google Apps Script, vous cr\xE9ez et manipulez des tableaux\
  \ associatifs (objets) en utilisant des accolades `{}`, en d\xE9finissant des paires\u2026"
lastmod: '2024-03-13T22:44:57.176983-06:00'
model: gpt-4-0125-preview
summary: "Dans Google Apps Script, vous cr\xE9ez et manipulez des tableaux associatifs\
  \ (objets) en utilisant des accolades `{}`, en d\xE9finissant des paires cl\xE9\
  -valeur \xE0 l'int\xE9rieur."
title: Utilisation des tableaux associatifs
weight: 15
---

## Comment :
Dans Google Apps Script, vous créez et manipulez des tableaux associatifs (objets) en utilisant des accolades `{}`, en définissant des paires clé-valeur à l'intérieur. Les clés sont des identifiants uniques, et les valeurs peuvent être tout, des chaînes et des nombres à des objets plus complexes ou des fonctions. Voici un exemple de base :

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Accéder aux valeurs
  Logger.log(user.name); // Affiche : John Doe
  Logger.log(user["email"]); // Affiche : johndoe@example.com

  // Ajouter de nouvelles paires clé-valeur
  user.title = "Développeur Logiciel";
  user["country"] = "USA";

  Logger.log(user.title); // Affiche : Développeur Logiciel

  // Itérer sur les paires clé-valeur
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

La sortie d'échantillon pour la partie itération pourrait ressembler à cela :
```
name: John Doe
age: 30
email: johndoe@example.com
title: Développeur Logiciel
country: USA
```

Notez comment vous pouvez utiliser à la fois la notation par point et la notation par crochets pour accéder et définir des propriétés. La notation par crochets est particulièrement utile lorsque vous travaillez avec des clés déterminées de manière dynamique ou incluant des caractères non admissibles dans les identifiants.

## Plongée Profonde
Les tableaux associatifs sous forme d'objets ont été une pierre angulaire de JavaScript et, par extension, de Google Apps Script, reflétant son mécanisme d'héritage basé sur les prototypes. Contrairement aux langues avec des tableaux associatifs traditionnels ou des dictionnaires (par exemple, le dict de Python), les objets Google Apps Script fournissent un moyen flexible et puissant de structurer les données, bénéficiant de la nature dynamique de JavaScript.

Il est important de noter, cependant, que la spécification ECMAScript 2015 a introduit les objets `Map` et `Set`, offrant une gestion des collections associatives plus directe avec certains avantages par rapport aux objets, tels que le maintien de l'ordre d'insertion et une meilleure performance pour les grands ensembles de données. Bien que Google Apps Script prenne également en charge ces derniers, le choix entre l'utilisation d'objets ou des structures `Map`/`Set` plus récentes dépend des besoins spécifiques et des considérations de performance. Pour la plupart des tâches de tableau associatif, les implémentations traditionnelles basées sur des objets fournissent une approche familière et polyvalente, mais il est conseillé d'examiner les alternatives plus récentes à mesure que la complexité de votre script augmente.
