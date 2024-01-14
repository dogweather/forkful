---
title:                "TypeScript: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes un développeur passionné et vous cherchez constamment de nouveaux défis et projets intéressants à entreprendre. Vous voulez peut-être apprendre une nouvelle technologie ou simplement étendre vos compétences en programmation. Quelle que soit la raison, démarrer un nouveau projet en TypeScript peut être une excellente décision pour progresser dans votre carrière.

## Comment faire

Avant de plonger dans le code, vous aurez besoin d'installer Node.js et le gestionnaire de paquets npm sur votre ordinateur. Une fois cela fait, vous pourrez installer TypeScript en utilisant la commande suivante dans votre terminal :

```
npm install -g typescript
```

Ensuite, créez un nouveau dossier pour votre projet et initialisez-le en tant que projet Node.js en exécutant la commande suivante :

```
npm init -y
```

Maintenant, vous pouvez vous concentrer sur le code en créant un fichier `index.ts` dans votre dossier de projet. Pour des fins de démonstration, voici un exemple de code TypeScript qui imprime "Bonjour, le monde !" dans la console :

```
// Importer le module 'console' de Node.js
import console from 'console';

// Function principale
function main() {
  // Imprimer le message
  console.log("Bonjour, le monde !");
}

// Appeler la function principale
main();
```

Ensuite, pour compiler votre code TypeScript en JavaScript, utilisez la commande suivante :

```
tsc index.ts
```

Cela générera un fichier `index.js` que vous pourrez exécuter en utilisant la commande suivante :

```
node index.js
```

Et voila ! Vous avez écrit et exécuté votre premier programme en TypeScript.

## Plongée plus profonde

Maintenant que vous avez une idée de base de la façon de démarrer un projet en TypeScript, vous pouvez explorer davantage en apprenant les concepts avancés tels que les types, les interfaces et les modules. Il existe également de nombreuses ressources en ligne, telles que des tutoriels et des cours, pour vous aider à apprendre TypeScript en profondeur.

N'oubliez pas que la pratique fait la perfection, alors n'hésitez pas à expérimenter et à essayer de nouveaux défis avec votre projet en TypeScript.

## Voir aussi

- [Site officiel de TypeScript](https://www.typescriptlang.org/)
- [Tutoriels TypeScript de Microsoft](https://docs.microsoft.com/fr-fr/archive/msdn-magazine/2019/september/typescript-get-to-know-the-language-and-tools)
- [Cours pour débutants en TypeScript](https://www.udemy.com/course/typescript-the-complete-developers-guide/)