---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Démarrer un nouveau projet consiste à lancer une nouvelle idée concrète, souvent en commençant à écrire des lignes de code. Les programmeurs le font pour explorer des idées nouvelles ou émergentes, résoudre des problèmes existants ou créer des solutions uniques.

## Comment faire:
Vous voudrez d'abord installer l'outil Node Package Manager (npm) pour gérer vos packages TypeScript. 

```TypeScript
npm install -g typescript  
```

Ensuite, créez un nouveau dossier pour votre projet et initialisez un nouveau projet npm.

```TypeScript
mkdir my_project  
cd my_project  
npm init -y  
```

Maintenant, nous allons créer un fichier .tsconfig pour configurer notre environnement de développement TypeScript.

```TypeScript
tsc --init  
```

Votre nouvel environnement de projet TypeScript est prêt à être utilisé! Vous pouvez désormais créer de nouveaux fichiers .ts et compiler en JavaScript en utilisant le command 'tsc'.

## Plongée profonde
Derrière ce processus relativement simple, il y a une histoire intéressante. TypeScript a été introduit en 2012 par Microsoft pour combler le manque de fonctionnalités orientées objet dans JavaScript. 

Il y a d'autres alternatives à TypeScript, y compris Babel et Elm. Cependant, TypeScript reste l'un des plus populaires en raison de ses caractéristiques et de la grande communauté qui le soutient.

Au cœur de TypeScript se trouve la création du fichier .tsconfig. Ce fichier vous permet de spécifier vos préférences de compiler, y compris la version du JavaScript que vous souhaitez cibler et les chemins de sortie des fichiers compilés.

## Voir aussi
Pour approfondir TypeScript et comment démarrer un nouveau projet, vous pouvez consulter les ressources suivantes :

- Documentation officielle TypeScript: https://www.typescriptlang.org/docs/
- Tutoriel vidéo sur YouTube: https://www.youtube.com/watch?v=BwuLxPH8IDs 
- Liste de ressources apprentissage sur GitHub: https://github.com/dzharii/awesome-typescript