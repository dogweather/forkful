---
title:                "Journalisation"
date:                  2024-01-26T01:02:03.261837-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Le logging est en quelque sorte l'équivalent logiciel du journal de bord d'un navire ; c'est un moyen d'enregistrer les événements qui se produisent pendant l'exécution d'une application. Les programmeurs le font pour garder une trace de ces événements pour le débogage, les pistes d'audit ou pour obtenir des informations sur le comportement d'un système en production.

## Comment faire :
Clojure s'appuie sur les installations de logging de Java, mais vous pouvez les exploiter de manière plus idiomatique avec Clojure. Voyons comment vous pourriez utiliser `clojure.tools.logging`, qui offre une abstraction simple sur plusieurs frameworks de logging :

Tout d'abord, ajoutez une dépendance pour `clojure.tools.logging` et une implémentation de logging telle que `log4j` dans votre `project.clj` :

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Maintenant, enregistrons quelques messages :

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Démarrage du calcul intense...")
  (Thread/sleep 3000) ; Simulation d'un long calcul
  (log/info "Calcul terminé. La réponse est 42.")
  42)

(compute-answer-to-everything)
```
La sortie ne montrera pas les messages `DEBUG` par défaut, car les niveaux de log sont typiquement réglés sur `INFO` :

```
INFO  [votre-namespace] - Calcul terminé. La réponse est 42.
```

Vous pouvez configurer les niveaux de log et les appenders dans un fichier `log4j.properties` pour obtenir une sortie plus verbeuse si nécessaire.

## Plongée en profondeur
`clojure.tools.logging` de Clojure existe depuis un certain temps et sert de pont entre le code Clojure et le monde du logging Java. Historiquement, Java a traversé plusieurs itérations et bibliothèques pour le logging telles que l'API de logging intégrée de Java, `log4j`, `slf4j`, et `logback`.

Dans Clojure, bien que vous puissiez utiliser directement les frameworks de logging Java, `clojure.tools.logging` détecte et délègue au framework de logging qu'il trouve dans votre classpath, vous évitant d'être étroitement lié à une implémentation spécifique. Cela peut aider à maintenir votre code Clojure plus portable et modulaire.

Les alternatives à `clojure.tools.logging` dans l'écosystème Clojure incluent des bibliothèques comme `timbre`, qui est une bibliothèque de logging pure Clojure avec des fonctionnalités comme la rotation des logs, le filtrage et le logging asynchrone prêtes à l'emploi.

Les détails d'implémentation sont cruciaux lorsqu'il s'agit de logging dans un environnement multi-threadé comme Clojure. Ici, l'immutabilité et la gestion des effets de bord offrent des avantages distincts. Le logging, en tant qu'effet de bord, doit être géré avec soin pour éviter les goulets d'étranglement de performance et assurer la sécurité des threads, ce que la plupart des frameworks de logging Java prennent déjà en charge.

Enfin, considérez le logging structuré, où les logs sont écrits sous forme de données structurées (comme JSON). Cela peut être extrêmement utile pour les analyses et traitements ultérieurs, en particulier lorsqu'il s'agit de systèmes distribués à grande échelle.

## Voir aussi
Si vous avez envie d'en savoir plus, envisagez de consulter ces ressources :

- Documentation de Clojure Tools Logging : https://github.com/clojure/tools.logging
- Timbre, une bibliothèque de logging Clojure : https://github.com/ptaoussanis/timbre
- Configuration de Log4J dans Clojure : http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Manuel de Logback pour des configurations avancées : http://logback.qos.ch/manual/
- Un guide sur le logging structuré dans Clojure : https://corfield.org/blog/2020/04/28/structured-logging/
