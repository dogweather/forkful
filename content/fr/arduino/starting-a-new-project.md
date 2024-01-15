---
title:                "Commencer un nouveau projet"
html_title:           "Arduino: Commencer un nouveau projet"
simple_title:         "Commencer un nouveau projet"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Se lancer dans un nouveau projet avec Arduino peut sembler intimidant, mais cela peut être une expérience très enrichissante. En utilisant simplement un microcontrôleur et du code, vous pourrez créer des projets interactifs et innovants qui peuvent résoudre des problèmes du quotidien ou simplement divertir.

## Comment faire

Pour commencer, il vous suffit d'assembler les différents composants du circuit en suivant un schéma électronique. Ensuite, vous pouvez écrire votre code en utilisant l'IDE (environnement de développement intégré) Arduino, qui offre une interface conviviale pour écrire, compiler et téléverser votre programme sur la carte Arduino.

Voici un exemple de code utilisant la LED intégrée de la carte Arduino :

```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(500);
  digitalWrite(LED_BUILTIN, LOW);
  delay(500);
}
```

Ce code allume la LED intégrée pendant une demi-seconde, puis l'éteint pendant une demi-seconde, créant ainsi un clignotement continu. Vous pouvez également utiliser des capteurs ou des actionneurs externes pour rendre votre projet plus interactif.

## Plongée en profondeur

Pour vraiment approfondir votre projet, vous pouvez également utiliser des shields, des modules supplémentaires qui se branchent sur la carte Arduino et ajoutent des fonctionnalités spécifiques, comme la communication sans fil, des interfaces utilisateur ou des capteurs spéciaux. Vous pouvez également utiliser des bibliothèques, des ensembles de fonctions préécrites qui vous aident à programmer plus facilement des tâches complexes telles que la communication avec des capteurs spécialisés.

De plus, la communauté Arduino est très active et il existe de nombreuses ressources en ligne pour apprendre et résoudre les problèmes courants. N'hésitez pas à explorer les forums de discussion, les tutoriels et les projets open-source pour trouver l'inspiration et obtenir de l'aide si nécessaire.

## Voir aussi

Pour plus d'informations sur les projets Arduino, voici quelques liens utiles :

- [Site officiel d'Arduino](https://www.arduino.cc/)
- [Forum Arduino](https://forum.arduino.cc/)
- [Tutoriels et projets Arduino](https://create.arduino.cc/projecthub)
- [Liste des librairies Arduino](https://www.arduinolibraries.info/)