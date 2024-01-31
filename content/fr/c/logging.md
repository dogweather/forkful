---
title:                "Journalisation"
date:                  2024-01-26T00:59:38.075288-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/logging.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La journalisation, ou logging en anglais, consiste essentiellement à consigner ce que fait votre programme, généralement en écrivant des messages dans un fichier ou un terminal. Les programmeurs le font pour suivre les événements, diagnostiquer des problèmes et pour avoir une trace d'audit qui raconte l'histoire du fonctionnement d'une application au fil du temps.

## Comment faire :
Commençons par quelques bases. C ne dispose pas d'un cadre intégré de logging, mais vous pouvez créer quelque chose de simple avec `stdio.h`. Voici comment :

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t maintenant;
    time(&maintenant);
    char *date = ctime(&maintenant);
    date[strlen(date) - 1] = '\0'; // Enlever le saut de ligne à la fin du résultat de ctime()
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("L'application a démarré.");
    // ... votre code ici ...
    logMessage("L'application fait quelque chose d'important.");
    // ... la suite de votre code ...
    logMessage("L'application est terminée.");
    return 0;
}
```

Un exemple de sortie pourrait ressembler à ceci :

```
[Tue Mar 9 12:00:01 2023] L'application a démarré.
[Tue Mar 9 12:00:02 2023] L'application fait quelque chose d'important.
[Tue Mar 9 12:00:03 2023] L'application est terminée.
```

Bien sûr, dans le monde réel, vous voudriez probablement écrire dans un fichier au lieu du terminal, gérer différents niveaux de logs et peut-être utiliser une bibliothèque prédéfinie.

## Plongée en Profondeur
Le logging en C a un charme désuet - il est aussi de bas niveau que la plupart du reste du langage. Historiquement, le logging était effectué en utilisant `fprintf` avec `stderr` ou un pointeur de fichier. À mesure que les programmes devenaient plus complexes, les besoins en matière de logging l'étaient également, ce qui a conduit au développement de bibliothèques telles que `syslog` sur les systèmes Unix, qui pouvaient gérer le logging provenant de multiples sources avec divers niveaux d'importance.

Dans le paysage moderne, il existe de nombreuses bibliothèques de logging en C, telles que `zlog`, `log4c`, et `glog`, qui offrent un ensemble riche de fonctionnalités incluant la rotation des logs, le logging structuré et le logging multithread. Ces solutions permettent un contrôle précis sur la verbosité des logs, les destinations et les formats.

Lors de la mise en place d'un système de logging, des détails tels que le formatage des horodatages, la gestion des fichiers de logs et la performance nécessitent une considération. L'horodatage des logs est crucial pour la corrélation des événements, tandis que la rotation des logs assure que les fichiers de logs ne consomment pas trop d'espace disque. Le processus de logging doit également être rapide et non bloquant pour le flux principal de l'application afin d'éviter que le logging ne devienne un goulot d'étranglement.

## Voir Aussi
Pour approfondir les bibliothèques et les pratiques de logging en C, consultez ces ressources :

- Manuel GNU `syslog` : https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog` : Une bibliothèque de logging très configurable pour C - https://github.com/HardySimpson/zlog
- `log4c` : Un cadre de logging pour C inspiré de Log4j - http://log4c.sourceforge.net/
- `glog` : La bibliothèque de logging de niveau application de Google - https://github.com/google/glog
