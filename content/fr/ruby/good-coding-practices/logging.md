---
title:                "Journalisation"
aliases:
- /fr/ruby/logging.md
date:                  2024-01-26T01:08:25.572424-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La journalisation dans la programmation, c'est comme tenir un journal intime pour votre application. Il s'agit de l'enregistrement systématique d'événements, de messages et de points de données qui vous donnent un aperçu de ce que votre application fait et de son comportement. Les développeurs font de la journalisation parce que c'est crucial pour le débogage, la surveillance de la santé de l'application et l'obtention d'indices sur des problèmes potentiels avant qu'ils ne se transforment en problèmes réels.

## Comment faire :
Ruby dispose d'un module intégré pour la journalisation, `Logger`, qui est super facile à utiliser. Voici un exemple rapide pour commencer :

```ruby
require 'logger'

# Créez un Logger qui affiche la sortie vers STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Messages de log d'exemple
logger.info("Ceci est un message d'info")
logger.warn("Ceci est un message d'avertissement")
logger.error("Ceci est un message d'erreur")
```

L'exécution du script ci-dessus produira quelque chose comme ceci :

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Ceci est un message d'info
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Ceci est un message d'avertissement
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Ceci est un message d'erreur
```

Vous pouvez configurer le format du journal et le niveau pour filtrer les bruits inutiles, et vous pouvez diriger les journaux vers différents sorties, comme un fichier ou même un service de journalisation externe.

## Approfondissement
La journalisation est comme une tradition séculaire en programmation. Historiquement, les journaux étaient de simples fichiers texte, analysés manuellement avec des outils comme `grep`. Mais le concept a évolué en un écosystème entier de cadres et de services de journalisation robustes tels que Log4j, Syslog sur Linux, ou Sematext et Loggly à l'ère du cloud.

Le `Logger` de Ruby est une manière simple de commencer, mais si vous avez besoin de plus de puissance et de flexibilité, vous pourriez envisager des alternatives comme Lograge ou Semantic Logger. Ces bibliothèques s'intègrent bien avec les applications Ruby, offrant un contrôle plus granulaire sur le formatage des journaux, y compris les journaux structurés (format JSON), de meilleures performances et une intégration transparente avec d'autres services.

Chaque bibliothèque de journalisation Ruby a sa propre façon de faire les choses, mais sous le capot, elles tournent toutes autour de l'idée d'une instance de logger à laquelle vous envoyez des messages. Le logger gère ces messages en fonction des niveaux définis — DEBUG, INFO, WARN, ERROR, FATAL et UNKNOWN — et décide de ce qu'il faut faire avec : les imprimer, les sauvegarder dans un fichier, les envoyer sur le réseau, etc.

## Voir aussi
Pour un approfondissement du module de journalisation intégré de Ruby, consultez la documentation officielle :

Si vous êtes intéressé par une journalisation plus avancée ou souhaitez explorer des gemmes tierces :
- [Lograge](https://github.com/roidrage/lograge)

Pour des pratiques et philosophies de journalisation générales (non spécifiques à Ruby), ces articles sont des lectures intemporelles :
- [Livre d'ingénierie de fiabilité de site de Google - Chapitre 16 : Gestion de la surcharge](https://sre.google/sre-book/handling-overload/#log-messages)
- [L'application 12 facteurs - Journaux](https://12factor.net/logs)
