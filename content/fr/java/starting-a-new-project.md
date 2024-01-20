---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Java"
category:             "Java"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

# L'Art de Démarrer un Nouveau Projet Java

## Quoi et Pourquoi?
Commencer un nouveau projet, c'est comme mettre la première pierre de votre immeuble. Les programmeurs font cela pour créer de nouvelles applications, résoudre des problèmes et élargir leurs compétences.

## Comment : 
Voici une ébauche de base d'un programme Java, à l'aide de l'IDE IntelliJ :

```Java
public class Main {
  public static void main(String[] args) {
    System.out.println("Hello, World!");
  }
}
```

Lorsque vous exécutez ce programme, le message "Hello, World!" s'affiche.

```Java
Hello, World!
```

Maintenant, ajoutons une simple fonction de salutation à notre programme :

```Java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
        greet("Alice");
    }

    public static void greet(String name) {
        System.out.println("Bonjour, " + name + "!");
    }
}
```

Voici ce qui s'affiche à l'exécution :

```Java
Hello, World!
Bonjour, Alice!
```

## Plongée Profonde
Historiquement, créer un projet Java était un processus manuel compliqué. Avec l'IDE moderne, le processus de démarrage d'un nouveau projet est fortement automatisé.

Il existe d'autres alternatives pour démarrer un nouveau projet, comme l'utilisation de ligne de commande, mais elles sont généralement plus techniques et invitant aux erreurs. 

En ce qui concerne les détails de mise en œuvre, l'IDE crée un nouveau répertoire avec les sous-dossiers nécessaires, installe les librairies nécessaires, et génère un fichier de code source de base.

## Voir Aussi
Pour plus d'informations sur IntelliJ, consultez leur [documentation officielle](https://www.jetbrains.com/help/idea/creating-and-running-your-first-java-application.html). 

Si vous préférez démarrer un nouveau projet en utilisant la ligne de commande, voici un [guide pratique](https://www.instructables.com/How-to-Create-Your-First-Java-Program-on-Ubuntu-Fl/). 

Pour une liste de librairies Java utiles à inclure dans votre projet, visitez [Awesome Java](https://java.libhunt.com/).