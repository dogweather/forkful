---
date: 2024-01-26 01:08:03.616886-07:00
description: "La journalisation est le processus d'enregistrement des comportements,\
  \ des erreurs et d'autres informations importantes d'une application dans un support\u2026"
lastmod: '2024-03-13T22:44:58.229491-06:00'
model: gpt-4-1106-preview
summary: "La journalisation est le processus d'enregistrement des comportements, des\
  \ erreurs et d'autres informations importantes d'une application dans un support\u2026"
title: Journalisation
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La journalisation est le processus d'enregistrement des comportements, des erreurs et d'autres informations importantes d'une application dans un support persistant, comme un fichier ou une base de données. Les programmeurs le font pour suivre la santé et les performances de leurs applications, pour déboguer des problèmes et pour garder un œil sur ce qui se passe sous le capot dans les environnements de production.

## Comment faire :
En Swift, vous pouvez écrire des logs dans la console avec des instructions `print` ou bien utiliser l'API `os.log` plus flexible, qui se connecte au système de journalisation unifié sur les plateformes Apple.

```Swift
import os.log

let logger = OSLog(subsystem: "com.votreapp.domaine", category: "network")

func fetchData() {
    // Simple instruction print
    print("Début de la récupération")
    
    // Journaliser un événement de niveau info avec os.log
    os_log(.info, log: logger, "Récupération de données depuis l'API.")
    
    do {
        let data = try performNetworkRequest()
        // Journaliser un événement de niveau debug
        os_log(.debug, log: logger, "Données reçues : %@", data.description)
    } catch {
        // Journaliser un événement de niveau erreur
        os_log(.error, log: logger, "Échec de la récupération de données : %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simuler une requête réseau
    return Data()
}
```

Un exemple de sortie sur la console pourrait ressembler à ceci :

```
Début de la récupération
Récupération de données depuis l'API.
Données reçues : Quelques octets de données...
```

Pour les erreurs, cela pourrait être :

```
Échec de la récupération de données : La connexion Internet semble être hors ligne.
```

## Approfondissement
La journalisation en Swift prend une nouvelle dimension de puissance et d'efficacité avec le système de journalisation unifié introduit dans iOS 10 et macOS Sierra. Contrairement à l'instruction `print` qui va directement à la console, ce système est basé sur des activités, et vous permet de filtrer les messages de journal en fonction de leur importance et s'ils sont destinés à des builds de débogage ou de publication.

Le contexte historique encadre l'évolution de la journalisation dans iOS et macOS, passant de simples instructions print vers des outils complets qui s'intègrent avec l'application Instruments et la Console, offrant des moyens sophistiqués pour analyser les logs.

Il existe une gamme d'alternatives à la journalisation au sein de Swift, telles que des bibliothèques tierces comme CocoaLumberjack, qui offre une couche macro par-dessus le système de journalisation unifié. Elle fournit un contrôle amélioré sur le formatage des logs, la gestion des fichiers et les options de performances.

Enfin, pour les détails de mise en œuvre ; OSLog est conçu pour être non seulement efficace mais aussi soucieux de la vie privée, avec la capacité de masquer les données privées lors de la journalisation. Il catégorise les logs en niveaux de faille, d'erreur, d'information et de débogage, chacun offrant une granularité différente pour le dépannage.

## Voir Aussi
- [Documentation sur la journalisation unifiée d'Apple](https://developer.apple.com/documentation/os/logging)
- [Tutoriel de journalisation Ray Wenderlich](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [Répertoire GitHub de CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
