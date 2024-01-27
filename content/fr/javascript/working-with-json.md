---
title:                "Manipulation de JSON"
date:                  2024-01-19
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
JSON signifie JavaScript Object Notation. C'est un format léger d'échange de données. Les programmeurs l'utilisent pour stocker et transporter des données car il est facile à comprendre pour l'homme et à analyser pour les machines.

## Comment faire :
Voici comment on gère JSON en JavaScript :

**Lire JSON :**
```javascript
const jsonString = '{"nom":"Jean", "age":30, "ville":"Paris"}';
const utilisateur = JSON.parse(jsonString);
console.log(utilisateur.nom);  // Affiche "Jean"
```

**Écrire JSON :**
```javascript
const utilisateur = { nom: "Jean", age: 30, ville: "Paris" };
const jsonString = JSON.stringify(utilisateur);
console.log(jsonString);  // Affiche '{"nom":"Jean","age":30,"ville":"Paris"}'
```

## Plongée Profonde
JSON a été introduit par Douglas Crockford au début des années 2000. Avant JSON, XML était le principal format d'échange de données, mais JSON a gagné en popularité grâce à sa simplicité. Malgré sa relation avec JavaScript, le format est indépendant du langage de programmation et beaucoup de langages disposent de leur propre analyseur JSON. En termes de performance, JSON est généralement plus rapide à analyser que XML.

## Voir Aussi
- [Mozilla JSON Documentation](https://developer.mozilla.org/fr/docs/Learn/JavaScript/Objects/JSON)
- [JSON.org](https://www.json.org/json-fr.html)
- [Douglas Crockford's JSON talk](https://www.youtube.com/watch?v=-C-JoyNuQJs)
