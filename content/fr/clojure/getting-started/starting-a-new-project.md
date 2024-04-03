---
date: 2024-01-20 18:03:22.413063-07:00
description: "Commencer un nouveau projet, c'est cr\xE9er une structure de base pour\
  \ d\xE9velopper une application. Les programmeurs font \xE7a pour organiser le code,\
  \ g\xE9rer les\u2026"
lastmod: '2024-03-13T22:44:57.282950-06:00'
model: gpt-4-1106-preview
summary: "Commencer un nouveau projet, c'est cr\xE9er une structure de base pour d\xE9\
  velopper une application."
title: Lancement d'un nouveau projet
weight: 1
---

## Quoi & Pourquoi ?
Commencer un nouveau projet, c'est créer une structure de base pour développer une application. Les programmeurs font ça pour organiser le code, gérer les dépendances et faciliter le démarrage rapide.

## Comment faire :
Pour initialiser un nouveau projet Clojure, on utilise Leiningen ou Clojure CLI-tools. Voici la marche à suivre avec Leiningen :

```Clojure
;; Installation de Leiningen :
;; Téléchargez le script `lein` à partir de https://leiningen.org/ et suivez les instructions d'installation.

;; Création d'un nouveau projet Clojure :
lein new app mon-projet

;; Structure du projet généré :
;; mon-projet/
;; ├── project.clj
;; ├── src/
;; │   └── mon_projet/
;; │       └── core.clj
;; ├── test/
;; │   └── mon_projet/
;; │       └── core_test.clj
;; ├── resources/
;; ├── target/
;; └── .gitignore
```

Avec Clojure CLI-tools, on fait comme ceci :

```Clojure
;; Installation de Clojure CLI-tools :
;; Suivez les instructions sur https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools.

;; Création d'un nouveau projet Clojure :
clj -X:new create :template app :name mon-projet

;; Structure du projet généré :
;; mon-projet/
;; ├── deps.edn
;; ├── src/
;; │   └── mon_projet/
;; │       └── core.clj
;; ├── test/
;; │   └── mon_projet/
;; │       └── core_test.clj
;; ├── resources/
;; ├── .gitignore
```

## Plongée profonde
Historiquement, Leiningen est l'outil de build de choix pour Clojure. Il automatise les tâches grâce à `project.clj`. Par contre, depuis Clojure 1.9, Clojure CLI-tools offre une approche plus minimaliste avec `deps.edn`, qui s'occupe uniquement des dépendances et de l'exécution des programmes. Les deux outils peuvent générer des gabarits de projet, gérer les dépendances et exécuter des tests. Le choix dépend de vos préférences et besoins : Leiningen pour une solution tout-en-un, Clojure CLI-tools pour un contrôle plus fin.

## À voir également
- Leiningen: [https://leiningen.org/](https://leiningen.org/)
- Clojure CLI-tools: [https://clojure.org/guides/deps_and_cli](https://clojure.org/guides/deps_and_cli)
- Guide de démarrage de Clojure: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
- Liste des templates Leiningen: [https://github.com/technomancy/leiningen/wiki/Templates](https://github.com/technomancy/leiningen/wiki/Templates)
