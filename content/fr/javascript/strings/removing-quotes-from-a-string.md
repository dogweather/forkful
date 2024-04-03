---
date: 2024-01-26 03:40:11.948520-07:00
description: "Comment faire : Imaginez que vous avez une cha\xEEne entour\xE9e de\
  \ guillemets doubles, comme `\"\\\"Bonjour, le monde !\\\"\"` et que vous voulez\
  \ le texte pur, sans\u2026"
lastmod: '2024-03-13T22:44:58.262199-06:00'
model: gpt-4-0125-preview
summary: "Imaginez que vous avez une cha\xEEne entour\xE9e de guillemets doubles,\
  \ comme `\"\\\"Bonjour, le monde !\\\"\"` et que vous voulez le texte pur, sans\
  \ citation."
title: "Retirer les guillemets d'une cha\xEEne"
weight: 9
---

## Comment faire :
Imaginez que vous avez une chaîne entourée de guillemets doubles, comme `"\"Bonjour, le monde !\""` et que vous voulez le texte pur, sans citation. Voici un petit extrait de code JavaScript pour libérer votre chaîne de ces chaînes de guillemets :

```javascript
let quotedString = "\"Bonjour, le monde !\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Sortie : Bonjour, le monde !
```

Et si vous traitez des guillemets simples ? Il suffit de modifier un peu l'expression régulière :

```javascript
let singleQuotedString = "'Bonjour, le monde !'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Sortie : Bonjour, le monde !
```

Ou si votre chaîne est un mix des deux ? Pas de problème :

```javascript
let mixedQuotedString = "\"'Bonjour, le monde !'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Sortie : 'Bonjour, le monde !'
```

## Approfondissement
Avant que JSON ne prenne le dessus, échapper les guillemets était un far west de backslashes et de bricolages. Les premiers langages de programmation n'étaient pas toujours compatibles avec les guillemets, ce qui signifiait beaucoup de manipulations manuelles de chaînes. Aujourd'hui, avec des formats de données standardisés, enlever les guillemets est souvent question de nettoyage des entrées avant qu'elles soient traitées comme du JSON ou de stockage de texte sans conflits de formatage.

Des alternatives à `.replace()` ? Bien sûr ! Vous pourriez diviser et joindre une chaîne sur des guillemets, utiliser slice si vous êtes certain de la position de vos guillemets, ou même matcher avec une expression régulière pour extraire le texte nécessaire. Tout dépend du contexte.

Mais n'oubliez pas les cas particuliers : guillemets à l'intérieur de guillemets, guillemets échappés, et caractères internationaux. Pensez à votre chaîne comme à un champ de mines potentiel d'exceptions, et procédez avec prudence. Les moteurs JavaScript modernes sont optimisés pour gérer efficacement les opérations d'expressions régulières, donc ils sont généralement la solution privilégiée, mais il vaut toujours la peine de vérifier la performance pour les tâches de traitement de données lourdes.

## Voir aussi
Explorez plus en profondeur la manipulation de chaînes et les regex :

- Mozilla Developer Network sur String.replace() : https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 pour tester vos modèles d'expression régulière : https://regex101.com/
- JSON.org pour comprendre pourquoi nous avons affaire à tant de guillemets dans le développement web moderne : http://json.org/
