---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire dans l'erreur standard (stderr) permet de séparer les messages d'erreurs du flux de sortie normal (stdout). Les programmeurs le font pour faciliter le débogage et la gestion des erreurs par les utilisateurs ou d'autres programmes.

## Comment ça marche :
```Swift
import Foundation

// Simple message d'erreur vers stderr
func ecrireErreurStandard(message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// Utilisation
ecrireErreurStandard(message: "Erreur: Fichier non trouvé.")

// Sortie attendue dans stderr:
// Erreur: Fichier non trouvé.
```

## Plongée Profonde
Historiquement, stderr est séparé de stdout pour permettre la redirection indépendante des messages d'erreur. Alternativement, on peut utiliser NSLog pour loguer les erreurs, mais cela va ajouter des métadonnées supplémentaires. L'implémentation en Swift fait souvent usage de `FileHandle` pour écrire dans stderr.

## À Voir Aussi
- Documentation Swift sur `FileHandle`: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filehandle)
- Guide complet sur le système de flux POSIX (stdout, stderr): [GNU I/O Streaming](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- Redirection des flux en ligne de commande Unix: [Redirection](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
