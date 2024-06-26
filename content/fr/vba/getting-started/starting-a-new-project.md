---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:47.105141-07:00
description: "Comment faire : Lorsque vous \xEAtes pr\xEAt \xE0 commencer un nouveau\
  \ projet VBA, le point de d\xE9part implique g\xE9n\xE9ralement d'acc\xE9der \xE0\
  \ l'\xE9diteur VBA et\u2026"
lastmod: '2024-03-13T22:44:57.574603-06:00'
model: gpt-4-0125-preview
summary: "Lorsque vous \xEAtes pr\xEAt \xE0 commencer un nouveau projet VBA, le point\
  \ de d\xE9part implique g\xE9n\xE9ralement d'acc\xE9der \xE0 l'\xE9diteur VBA et\
  \ d'initialiser votre cadre de projet."
title: "D\xE9marrer un nouveau projet"
weight: 1
---

## Comment faire :
Lorsque vous êtes prêt à commencer un nouveau projet VBA, le point de départ implique généralement d'accéder à l'éditeur VBA et d'initialiser votre cadre de projet. Explorons les étapes en utilisant Excel comme application hôte :

1. **Ouvrir l'éditeur VBA** : Dans Excel, appuyez sur `Alt + F11` pour accéder à l'éditeur VBA.
2. **Insérer un Nouveau Module** : Naviguez vers `Insérer > Module` depuis le menu pour ajouter un nouveau module à votre projet. C'est là que votre code résidera.
3. **Écrire Votre Première Macro** : Codons une macro simple qui affiche une boîte de message. Tapez le code suivant dans le module :

```vb
Sub SayHello()
    MsgBox "Bonjour, le monde !", vbInformation, "Salutations"
End Sub
```

4. **Exécuter Votre Macro** : Appuyez sur `F5` pendant que votre curseur est dans la sous-routine `SayHello` ou allez dans `Exécuter > Exécuter Sub/UserForm` et sélectionnez `SayHello`. Vous devriez voir apparaître une boîte de message avec "Bonjour, le monde !" et un bouton "OK".

Résultat Exemple :

```plaintext
Une boîte de message avec "Bonjour, le monde !" affiché.
```

5. **Sauvegarder Votre Projet** : Avant de sortir, assurez-vous de sauvegarder votre travail. Si votre classeur Excel était auparavant non enregistré, il vous sera demandé de sauvegarder en tant que classeur activé pour les macros (format de fichier `.xlsm`).

## Approfondissement
Visual Basic pour Applications est une pierre angulaire dans les stratégies d'automatisation de Microsoft depuis son introduction en 1993. Originant comme une évolution de son prédécesseur, MacroBasic, VBA a fourni une solution plus robuste avec une meilleure intégration à travers la suite Office de Microsoft. La transition vers VBA a été pivotale, marquant un tournant vers des capacités de script plus complexes qui exploitaient la puissance des langages de programmation à part entière.

Malgré son âge, VBA reste prévalent dans les environnements de bureau modernes, en grande partie grâce à son intégration profonde au sein des produits Office et à la vaste base de code hérité dans de nombreuses organisations. Cependant, il est important de noter que pour de nouvelles applications basées sur le web ou pour des tâches nécessitant plus de scalabilité et d'intégration avec des applications non Office, des langages et des cadres de travail comme Python, avec son riche écosystème de bibliothèques, ou JavaScript pour les scripts Office, offrent une approche plus moderne et polyvalente. Ces alternatives, bien qu'exigeant une courbe d'apprentissage plus raide et une configuration, fournissent une applicabilité plus large et un soutien pour les pratiques de développement contemporaines comme le contrôle de version et les pipelines de déploiement.
