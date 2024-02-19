---
aliases:
- /fr/c/logging/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:48.144656-07:00
description: "La journalisation en C consiste \xE0 enregistrer le flux et les \xE9\
  v\xE9nements notables d'un programme pendant son ex\xE9cution, fournissant un examen\
  \ tangible de\u2026"
lastmod: 2024-02-18 23:09:09.363646
model: gpt-4-0125-preview
summary: "La journalisation en C consiste \xE0 enregistrer le flux et les \xE9v\xE9\
  nements notables d'un programme pendant son ex\xE9cution, fournissant un examen\
  \ tangible de\u2026"
title: Journalisation
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La journalisation en C consiste à enregistrer le flux et les événements notables d'un programme pendant son exécution, fournissant un examen tangible de son comportement et de sa performance. Les programmeurs utilisent la journalisation à des fins de débogage, pour surveiller la santé du logiciel, et assurer la sécurité du système.

## Comment faire :

En C, la journalisation peut être réalisée avec des opérations de fichiers basiques ou en utilisant des bibliothèques plus sophistiquées. Pour simplifier, nous commencerons avec la bibliothèque d’E/S standard. Les extraits suivants montrent des implémentations de journalisation basiques.

Pour enregistrer des messages simples :

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Ouvre le fichier log en mode ajout
    
    if (logFile == NULL) {
        perror("Erreur lors de l'ouverture du fichier log.");
        return -1;
    }
    
    fprintf(logFile, "Démarrage de l'application.\n");
    
    // Votre logique d'application ici
    
    fprintf(logFile, "Application terminée avec succès.\n");
    fclose(logFile);
    
    return 0;
}
```

Sortie dans `application.log` :

```
Démarrage de l'application.
Application terminée avec succès.
```

Pour inclure des journaux plus détaillés avec des horodatages et des niveaux de log :

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // Supprime le caractère de retour à la ligne
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Erreur lors de l'ouverture du fichier log.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Démarrage de l'application");
    // Votre logique d'application ici
    logMessage(logFile, "ERROR", "Un exemple d'erreur");
    
    fclose(logFile);
    
    return 0;
}
```

Sortie dans `detailed.log` :

```
[Jeu Mar 10 14:32:01 2023] INFO - Démarrage de l'application
[Jeu Mar 10 14:32:02 2023] ERROR - Un exemple d'erreur
```

## Approfondissement

La journalisation en C, comme démontré, repose sur des opérations de fichiers simples, ce qui est efficace mais pas aussi puissant ou flexible que les installations de journalisation dans d'autres langages, comme le module `logging` de Python ou `Log4j` de Java. Pour des capacités de journalisation plus avancées en C, les développeurs se tournent souvent vers des bibliothèques comme `syslog` sur les systèmes de type Unix, qui fournit une gestion de journalisation à l'échelle du système, ou vers des bibliothèques tierces comme `log4c`.

Historiquement, la journalisation a été une partie intégrante de la programmation, remontant aux pratiques de programmation précoces où le suivi et la compréhension du flux et des erreurs du programme étaient principalement réalisés par des impressions physiques. Au fur et à mesure que les systèmes évoluaient, la journalisation est devenue plus sophistiquée, supportant maintenant différents niveaux de gravité, la rotation des logs, et la journalisation asynchrone.

Alors que la bibliothèque standard C fournit les outils basiques pour implémenter la journalisation, ses limitations conduisent souvent à la création de cadres de journalisation personnalisés ou à l'adoption de bibliothèques externes pour des solutions de journalisation plus riches en fonctionnalités et flexibles. Malgré ces limitations, comprendre et mettre en œuvre la journalisation basique en C est crucial pour le débogage et la maintenance des logiciels, en particulier dans des environnements où il convient de minimiser les dépendances externes.
