---
title:                "Journalisation"
date:                  2024-01-26T01:08:20.655601-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Le logging est le processus d'enregistrement des événements, erreurs et autres informations importantes pendant l'exécution d'un programme sur un support externe, souvent des fichiers ou des bases de données. Les programmeurs utilisent les logs pour surveiller le comportement des logiciels, déboguer les problèmes et suivre les activités du système pour l'analyse de la sécurité et de la performance.

## Comment faire :

En TypeScript, vous pouvez facilement mettre en œuvre un logging de base en utilisant les méthodes de la console ou intégrer un logging plus avancé avec des bibliothèques telles que `winston` ou `pino`. Voici un exemple de base utilisant `console.log` et un autre plus avancé avec `winston`.

```TypeScript
// Logging de base dans la console
console.log('Info : Démarrage de l'application...');
console.error('Erreur : Impossible de récupérer les données.');

// Sortie d'échantillon
// Info : Démarrage de l'application...
// Erreur : Impossible de récupérer les données.
```

Pour un logging plus robuste, configurons `winston` :

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level} : ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Serveur démarré !');
logger.warn('Avertissement : espace disque faible.');
logger.error('Échec de la connexion à la base de données.');

// Sortie d'échantillon dans combined.log
// 2023-01-20 14:42:07 info : Serveur démarré !
// 2023-01-20 14:42:09 warn : Avertissement : espace disque faible.
// 2023-01-20 14:42:12 error : Échec de la connexion à la base de données.
```

## Plongée en profondeur :

Le concept du logging dans le contexte informatique remonte aux premiers jours de la programmation, où le terme lui-même est dérivé du "journal de bord", un système de tenue de registres nautique. Historiquement, les événements des programmes étaient souvent enregistrés sur des impressions physiques ou des sorties de terminal, en particulier durant l'ère des mainframes.

Aujourd'hui, vous disposez d'une multitude d'outils et de bibliothèques adaptés à divers besoins de logging, allant de simples fichiers texte à des systèmes de gestion de logs complexes. Parmi les alternatives à `winston`, il y a `pino`, qui se vante de hautes performances, et `Bunyan`, qui est basé sur du JSON. Lorsqu’on travaille avec Node.js, les bibliothèques de logging offrent souvent des mécanismes de flux pour acheminer les logs vers différentes destinations, une prise en charge de la rotation des logs et des formateurs personnalisables.

En termes de mise en œuvre, les messages de log contiennent généralement un horodatage, un niveau de gravité (tel que info, warn, error) et le message réel. Une bonne pratique de logging recommande de catégoriser correctement les niveaux de logs, d'éviter les données sensibles dans les logs et de prendre en compte les implications sur la performance dans les applications à haut débit.

## Voir également :

- [Winston - Un logger pour presque tout](https://www.npmjs.com/package/winston)
- [Pino - Logger pour Node.js à très faible surcharge](https://www.npmjs.com/package/pino)
- [Meilleures pratiques de logging avec Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [L'application en 12 facteurs - Logs](https://12factor.net/logs)
