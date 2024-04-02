---
date: 2024-01-20 18:03:51.148272-07:00
description: "Lancer un nouveau projet Java, c'est cr\xE9er un espace o\xF9 votre\
  \ code va prendre vie. On le fait pour transformer une id\xE9e en une application\
  \ qui peut\u2026"
lastmod: '2024-03-13T22:44:57.640789-06:00'
model: gpt-4-1106-preview
summary: "Lancer un nouveau projet Java, c'est cr\xE9er un espace o\xF9 votre code\
  \ va prendre vie. On le fait pour transformer une id\xE9e en une application qui\
  \ peut\u2026"
title: Lancement d'un nouveau projet
weight: 1
---

## Quoi et Pourquoi ?
Lancer un nouveau projet Java, c'est créer un espace où votre code va prendre vie. On le fait pour transformer une idée en une application qui peut tourner sur des millions de machines.

## Comment faire :

Créer un projet simple avec `javac` et `java`:

```Java
// Hello.java
public class Hello {
    public static void main(String[] args) {
        System.out.println("Salut les devs !");
    }
}

// Compilons le fichier
// Ouvrir le terminal, puis taper:
// $ javac Hello.java

// Exécuter le programme compilé avec `java`
// Dans le terminal, entrer:
// $ java Hello
```

Résultat attendu:

```Shell
Salut les devs !
```

Utilisation de Maven pour un projet plus structuré:

```Shell
// Installer Maven puis dans le terminal
$ mvn archetype:generate -DgroupId=com.votrepack -DartifactId=NomDuProjet -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false

// Changez de répertoire pour aller dans le nouveau dossier créé
$ cd NomDuProjet

// Compiler et exécuter avec Maven
$ mvn package
$ java -cp target/NomDuProjet-1.0-SNAPSHOT.jar com.votrepack.App
```

## Plongée Profonde

Avant, créer un projet Java pouvait sembler fastidieux. Maintenant, des outils comme Maven ou Gradle simplifient cette tâche. Maven, par exemple, gère les dépendances, construit le projet, et plus encore. Historiquement, `javac` et `java` étaient les outils de base pour compiler et exécuter des fichiers `.java`. On peut toujours les utiliser pour un petit projet ou pour apprendre les bases.

Pourquoi choisir Maven? C'est un standard de facto, très documenté et soutenu par une large communauté. Gradle, son alternative moderne, brille par sa flexibilité et sa vitesse grâce à un langage de configuration Groovy ou Kotlin basé.

Le choix dépend de vos préférences, de la taille de votre projet et de ce que votre équipe utilise déjà.

## Voir Aussi

- Documentation officielle de Maven : [https://maven.apache.org/guides/index.html](https://maven.apache.org/guides/index.html)
- Tutoriel officiel de Java : [https://docs.oracle.com/javase/tutorial/](https://docs.oracle.com/javase/tutorial/)
- Guide de démarrage de Gradle : [https://gradle.org/guides/](https://gradle.org/guides/)
