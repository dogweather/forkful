---
title:                "Journalisation"
aliases: - /fr/kotlin/logging.md
date:                  2024-01-26T01:07:28.410978-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/logging.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

La journalisation, dans son essence, est la pratique d'enregistrer des événements et des données provenant d'une application logicielle vers une sortie externe, comme un fichier ou une console. Les programmeurs créent des journaux pour suivre le code, résoudre des problèmes et surveiller le comportement d'une application en conditions réelles, fournissant des informations critiques qui ne peuvent pas être obtenues aussi efficacement d'une autre manière.

## Comment procéder :

En Kotlin, la journalisation peut être réalisée en utilisant la fonction intégrée `println()` pour des cas simples, ou avec des bibliothèques plus sophistiquées comme SLF4J avec Logback ou Log4j pour des besoins avancés.

Voici un exemple simple utilisant `println()` :

```Kotlin
fun main() {
    println("Message de journalisation simple : Application démarrée.")
    // ... ici une logique d'application ...
    try {
        // Simuler une erreur
        throw Exception("Erreur simulée")
    } catch (e: Exception) {
        println("Message de journalisation d'erreur : " + e.message)
    }
}
```

Sortie :
```
Message de journalisation simple : Application démarrée.
Message de journalisation d'erreur : Erreur simulée
```

Et voici un extrait utilisant SLF4J avec Logback configuré :

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Message de journalisation structuré : Appli lancée.")
    // ... ici une logique d'application ...
    try {
        // Simuler une erreur
        throw Exception("Erreur simulée")
    } catch (e: Exception) {
        logger.error("Journal d'erreurs structuré : ", e)
    }
}
```

En supposant la configuration appropriée de Logback, la sortie serait formatée et pourrait ressembler à ceci lorsqu'elle est écrite dans un fichier de journalisation :
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Message de journalisation structuré : Appli lancée.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Journal d'erreurs structuré : 
java.lang.Exception: Erreur simulée
   à com.myapp.Main.main(Main.kt:10)
```

## Exploration approfondie

Historiquement, la journalisation dans les logiciels s'est développée parallèlement à la complexité croissante des applications et des systèmes. Des instructions d'impression simples étaient suffisantes pour les premiers jours, où les programmes étaient souvent exécutés et débogués par le développeur lui-même. Mais alors que les systèmes se connectaient en réseau et fonctionnaient dans différents environnements et pour différents utilisateurs, un système de journalisation robuste et persistant est devenu crucial.

Avant que Kotlin ne devienne populaire, les développeurs Java ont largement adopté des bibliothèques comme Log4j et plus tard SLF4J. Ceux-ci ont inspiré des pratiques similaires en Kotlin, exploitant l'interopérabilité de Kotlin avec les bibliothèques Java. SLF4J agit comme une couche d'abstraction, permettant à l'implémentation de journalisation actuelle d'être échangée - généralement Logback ou Log4j2 sont les choix préférés.

Kotlin permet également des solutions de journalisation multiplateforme fonctionnant à travers JVM, JavaScript et Natif, par exemple, à travers le mécanisme `expect`/`actual`, qui abstrait les implémentations spécifiques à la plateforme.

Contrairement aux bibliothèques de journalisation dédiées, println persiste comme la forme la plus simple de journalisation car elle ne nécessite pas de configuration ou de dépendances supplémentaires ; cependant, elle est généralement inadaptée pour les applications en production en raison de son absence de fonctionnalités telles que les niveaux de journalisation, la rotation des journaux et les formats structurés.

Parmi les autres caractéristiques communes des cadres de journalisation avancés, on trouve :

- Les niveaux de journal (DEBUG, INFO, WARN, ERROR, etc.) pour catégoriser l'urgence des messages de journalisation.
- La sortie vers diverses destinations, comme la console, les fichiers, les bases de données ou les services réseau.
- Rotation automatique des journaux et politiques de rétention.
- Support du tracing distribué pour l'architecture de microservices.
- Journalisation structurée utilisant des formats comme JSON, ce qui s'intègre bien avec les systèmes d'analyse de journaux.

Ces outils et fonctionnalités sont essentiels pour maintenir un système fiable et observable, en particulier dans des environnements complexes, distribués ou hautement scalés.

## Voir également

Pour approfondir et obtenir plus d'informations sur la journalisation en Kotlin, consultez :

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, le successeur de Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Documentation Kotlin Multiplateforme sur les déclarations 'expect' et 'actual' : [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Un guide pour la journalisation structurée en Kotlin : [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
