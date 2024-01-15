---
title:                "Concaténation de chaînes"
html_title:           "Arduino: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être remarqué qu'il était souvent nécessaire de combiner plusieurs chaînes de caractères dans vos programmes Arduino. Cela peut sembler un peu fastidieux, mais c'est en réalité une tâche importante pour créer des messages clairs et complets dans vos projets.

## Comment faire

Pour combiner des chaînes de caractères, nous utiliserons la fonction `concat()` en Arduino. Cette fonction prend comme paramètres les différentes chaînes que vous souhaitez concaténer, dans l'ordre dans lequel vous souhaitez qu'elles apparaissent dans la chaîne finale. Voyons un exemple :

```Arduino
String nom = "Alexandre";
String surnom = "le Maker";
String message = "";

message.concat("Bonjour, je suis ");
message.concat(nom);
message.concat(", aussi connu sous le nom de ");
message.concat(surnom);
```

Dans cet exemple, nous utilisons d'abord `concat()` pour ajouter le début du message, puis nous ajoutons le nom et enfin le surnom. Le résultat final sera : `Bonjour, je suis Alexandre, aussi connu sous le nom de le Maker.`

## Plongée en profondeur

La fonction `concat()` est utile pour de nombreux scénarios, mais il est important de noter qu'elle a des limites. Par exemple, il n'est pas possible de concaténer plus de 256 caractères à la fois, sinon vous risquez de vous retrouver avec des valeurs de chaîne incorrectes.

De plus, il est important de garder à l'esprit que la concaténation de chaînes peut prendre du temps et peut ralentir vos programmes, surtout si vous avez besoin de concaténer plusieurs chaînes à la fois. Dans ce cas, il peut être préférable d'utiliser des tableaux de caractères plutôt que des chaînes de caractères.

## Voir aussi

- [Documentation officielle sur la fonction `concat()` en Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Tutoriel sur la manipulation des chaînes de caractères en Arduino](https://www.instructables.com/A-Guide-to-Arduino-Strings/)