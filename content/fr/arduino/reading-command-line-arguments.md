---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Lire les arguments de la ligne de commande signifie interpréter les options d'input passées lors du lancement d'un programme. Les programmeurs le font pour rendre leurs logiciels plus flexibles et dynamiques.

## Comment faire:

Jetez un œil à notre exemple de code. Arduino ne supporte pas directement les arguments de ligne de commande, mais un moyen similaire d'atteindre ce comportement consiste à passer et lire des données provenant du port série.

```Arduino
String input;

void setup() {
  Serial.begin(9600);
}

void loop() {
  while(Serial.available()){
    char c = Serial.read();
    input += c;
  }
  
  if (input.length() > 0) {
    Serial.println(input);
    input = "";
  }
}
```

Dans cet exemple, tous les caractères entrants sur le port série sont lus et stockés dans une chaîne de caractères. Si une chaîne a été construite, elle est imprimée et la mémoire est ensuite effacée.

## Deep Dive:

Historiquement, la lecture des arguments de ligne de commande est une pratique qui remonte aux jours de Unix. Elle est couramment utilisée dans les langages de programmation basés sur le texte comme C, Perl, et Python.

Les microcontrôleurs comme Arduino n'ont pas d'accès direct à une ligne de commande, il faut donc utiliser d'autres techniques pour obtenir une fonctionnalité similaire.

En alternance, vous pourriez considérer l'utilisation d'un parseur pour lire les commandes spécifiques ou données de configuration depuis une carte SD ou même utiliser un bouclier Ethernet pour recevoir les commandes depuis un serveur web.

## Voir aussi:

Visitez le site officiel d'Arduino [ici](https://www.arduino.cc/) pour plus d'informations et de guides sur le travail avec les Arduinos.

Consultez le [forum de la communauté](https://forum.arduino.cc/) d'Arduino pour plus d'idées sur les projets et les questions techniques.

Enfin, pour une exploration plus approfondie des arguments de ligne de commande dans le contexte de la programmation Unix, voyez [cet article](https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html) de la Single UNIX Specification.