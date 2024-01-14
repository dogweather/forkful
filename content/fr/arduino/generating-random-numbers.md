---
title:    "Arduino: Génération de nombres aléatoires"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un amateur de programmation et que vous possédez un Arduino, vous avez sûrement déjà entendu parler de la fonction "random()" qui génère des nombres aléatoires. Mais pourquoi générer des nombres aléatoires est-il important ? Eh bien, cela peut être utile pour de nombreuses raisons telles que créer des jeux, des simulations, des générateurs de mots de passe ou même aléatoires et bien plus encore ! Dans cet article, nous allons vous montrer comment générer des nombres aléatoires avec votre Arduino.

## Comment faire

Pour générer des nombres aléatoires avec votre Arduino, vous aurez besoin de la fonction "random()". Voici un exemple de code qui génère un nombre aléatoire entre 0 et 10 :

```Arduino
int nombre = random(0, 10);
Serial.println(nombre);
```

Ce code va générer un nombre aléatoire entre 0 et 10 et l'afficher sur le moniteur série de votre Arduino. N'oubliez pas d'ajouter "randomSeed(analogRead(0));" dans votre fonction "setup()" pour initialiser la fonction "random()" avec une nouvelle graine à chaque fois que vous allumez votre Arduino. Sinon, vous obtiendrez le même nombre aléatoire à chaque fois.

Vous pouvez également générer des nombres aléatoires dans un tableau en utilisant une boucle "for" comme dans l'exemple ci-dessous :

```Arduino
int nombres[5];

for (int i = 0; i < 5; i++) {
  nombres[i] = random(0, 10);
  Serial.println(nombres[i]);
}
```

Ce code va générer 5 nombres aléatoires entre 0 et 10 et les stocker dans le tableau "nombres". Ensuite, il les affichera sur le moniteur série.

## Plongée en profondeur

La fonction "random()" utilise une formule mathématique appelée "pseudorandom" qui génère des nombres en apparence aléatoires mais qui sont en fait déterministes. Cela signifie que si vous connaissez la formule et la graine initiale, vous pouvez deviner les nombres générés.

Pour éviter cela, il est recommandé d'utiliser une graine aléatoire plutôt qu'une valeur fixe. Vous pouvez en trouver une en utilisant le port analogique de votre Arduino. De plus, vous pouvez également utiliser des capteurs externes tels que des photorésistances ou des thermistances pour générer une graine encore plus aléatoire.

## Voir aussi

Pour en savoir plus sur la fonction "random()", vous pouvez consulter ces liens :

- [Documentation officielle de la fonction random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/ "Documentation officielle de la fonction random()")
- [Générateur de nombres aléatoires à haute qualité pour Arduino](https://ntrs.nasa.gov/search.jsp?R=20150000620 "Générateur de nombres aléatoires à haute qualité pour Arduino")
- [Utiliser un Arduino pour générer des nombres aléatoires véritables](https://learn.adafruit.com/random-awesomeness-with-an-arduino/using-the-arduino-to-actually-make-real-random-numbers "Utiliser un Arduino pour générer des nombres aléatoires véritables")

Maintenant que vous savez comment générer des nombres aléatoires avec votre Arduino, il est temps de s'amuser à les utiliser dans vos projets !