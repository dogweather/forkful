---
title:                "Calcul d'une date future ou passée"
aliases:
- /fr/typescript/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:32:22.798814-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcul d'une date future ou passée"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou dans le passé, c'est juste des maths avec des jours, des mois, des années. Les devs manipulent des dates pour planifier des événements, des rappels, ou juste pour voir combien de temps s'est écoulé.

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
