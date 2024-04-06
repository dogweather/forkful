---
date: 2024-01-20 17:32:22.798814-07:00
description: "Comment faire : Historiquement, les dates n'\xE9taient pas si simples\
  \ \xE0 g\xE9rer, surtout \xE0 cause des fuseaux horaires et des formats. TypeScript\
  \ est bas\xE9 sur\u2026"
lastmod: '2024-04-05T21:53:59.014448-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, les dates n'\xE9taient pas si simples \xE0 g\xE9rer, surtout\
  \ \xE0 cause des fuseaux horaires et des formats."
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
```TypeScript
// Calculer une date 10 jours dans le futur
const ajouterJours = (date: Date, jours: number): Date => {
  const resultat = new Date(date);
  resultat.setDate(resultat.getDate() + jours);
  return resultat;
};

// Usage
const maintenant = new Date();
const dansDixJours = ajouterJours(maintenant, 10);
console.log(dansDixJours);
// Sortie attendue : [Date 10 jours après la date courante]

// Calculer une date 5 ans dans le passé
const retirerAns = (date: Date, ans: number): Date => {
  const resultat = new Date(date);
  resultat.setFullYear(resultat.getFullYear() - ans);
  return resultat;
};

// Usage
const ilYaCinqAns = retirerAns(maintenant, 5);
console.log(ilYaCinqAns);
// Sortie attendue : [Date 5 ans avant la date courante]
```

## Plongée en profondeur
Historiquement, les dates n'étaient pas si simples à gérer, surtout à cause des fuseaux horaires et des formats. TypeScript est basé sur JavaScript, qui offre des objets `Date` assez robustes pour manipuler des dates et des heures. Il existe des bibliothèques tierces comme `moment.js` ou `date-fns` qui offrent plus de fonctions et de facilité, mais elles ajoutent une dépendance à votre projet. Parfois, les `Date` natives de JS (et donc TypeScript) suffisent.

L'implémentation doit considérer les années bissextiles, les changements d'heure et les incohérences entre les mois. TypeScript ne règle pas ces problèmes du jour au lendemain, mais il aide à les gérer en typant correctement les entrées et les sorties.

## Voir aussi
- MDN Web Docs pour `Date`: [Lien](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Documentation de TypeScript: [Lien](https://www.typescriptlang.org/docs/)
- `moment.js`: [Lien](https://momentjs.com/)
- `date-fns`: [Lien](https://date-fns.org/)
