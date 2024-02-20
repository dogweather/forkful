---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:47.483054-07:00
description: "YAML, qui signifie YAML Ain't Markup Language (YAML n'est pas un langage\
  \ de balisage), est un standard de s\xE9rialisation de donn\xE9es convivial pour\
  \ tous les\u2026"
lastmod: 2024-02-19 22:05:16.898065
model: gpt-4-0125-preview
summary: "YAML, qui signifie YAML Ain't Markup Language (YAML n'est pas un langage\
  \ de balisage), est un standard de s\xE9rialisation de donn\xE9es convivial pour\
  \ tous les\u2026"
title: Travailler avec YAML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
YAML, qui signifie YAML Ain't Markup Language (YAML n'est pas un langage de balisage), est un standard de sérialisation de données convivial pour tous les langages de programmation. Les programmeurs l'utilisent pour les fichiers de configuration, la messagerie inter-processus et le stockage de données car sa lisibilité est beaucoup plus proche de l'anglais courant comparée à d'autres formats de données comme XML ou JSON, ce qui le rend plus simple à comprendre et à écrire.

## Comment faire :
Swift n'inclut pas de support intégré pour l'analyse et la sérialisation YAML, nécessitant l'utilisation de bibliothèques tierces. Un choix populaire est `Yams`, une bibliothèque pour travailler avec YAML en Swift.

D'abord, vous devez ajouter `Yams` à votre projet. Si vous utilisez le Swift Package Manager, vous pouvez l'ajouter comme une dépendance dans votre fichier `Package.swift` :

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### Analyser le YAML en Swift
Supposons que vous ayez la configuration YAML suivante pour une application simple :

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

Voici comment vous pouvez analyser cette chaîne YAML en Swift en utilisant `Yams` :

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // Exemple d'accès aux données analysées
        if let name = data["name"] as? String {
            print("Nom de l'App : \(name)")
        }
    }
} catch {
    print("Erreur lors de l'analyse YAML : \(error)")
}
```

Sortie d'exemple :

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
Nom de l'App : MyApp
```

### Sérialiser les objets Swift en YAML
Convertir un objet Swift en chaîne YAML est également simple avec `Yams`. Supposons que vous ayez la même structure de données qui doit être sérialisée :

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("Erreur de sérialisation en YAML : \(error)")
}
```

Cela produira une chaîne formatée YAML :

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

Ces exemples démontrent les opérations de base pour travailler avec YAML dans les applications Swift. Rappelez-vous, alors que YAML excelle en lisibilité humaine et facilité d'utilisation, considérez toujours les besoins spécifiques de votre application, en particulier en ce qui concerne la performance et la complexité, lors du choix de votre format de sérialisation de données.
