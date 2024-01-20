---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Les expressions régulières permettent de chercher, filtrer et manipuler des textes avec précision. Les programmeurs s'en servent pour faciliter la validation, l'extraction et le traitement des données textuelles.

## How to:

```TypeScript
// Exemple simple pour valider un format d'email
const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
const emailToTest = 'exemple@domaine.com';

console.log(emailRegex.test(emailToTest)); // Sortie: true

// Remplacer toutes les occurrences de "chat" par "chien"
const sentence = "Le chat chasse le chaton.";
const replacedSentence = sentence.replace(/chat/g, "chien");

console.log(replacedSentence); // Sortie: "Le chien chasse le chienon."
```

## Deep Dive
Les expressions régulières existent depuis les années 1950, développées à l'origine dans la théorie des automates. JavaScript, et donc TypeScript, utilisent la syntaxe des expressions régulières de Perl. Des alternatives incluent des parsers dédiés ou des librairies pour des cas d'utilisation complexes. Côté implémentation, RegExp en TypeScript est un objet qui peut être créé littéralement `/pattern/flags` ou via le constructeur `new RegExp('pattern', 'flags')`.

## See Also
- [MDN Regular Expressions Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)