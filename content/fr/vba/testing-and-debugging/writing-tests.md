---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:44.884022-07:00
description: "\xC9crire des tests en programmation implique de cr\xE9er des proc\xE9\
  dures sp\xE9cifiques pour valider la fonctionnalit\xE9 et la performance de vos\
  \ segments de code,\u2026"
lastmod: '2024-03-13T22:44:57.582256-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire des tests en programmation implique de cr\xE9er des proc\xE9dures\
  \ sp\xE9cifiques pour valider la fonctionnalit\xE9 et la performance de vos segments\
  \ de code,\u2026"
title: "R\xE9daction de tests"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire des tests en programmation implique de créer des procédures spécifiques pour valider la fonctionnalité et la performance de vos segments de code, en s'assurant qu'ils fonctionnent comme prévu dans diverses conditions. Les programmeurs le font pour détecter les bugs tôt, améliorer la qualité du code et faciliter la maintenance future du code et ses améliorations.

## Comment faire :

Bien que Visual Basic pour Applications (VBA) ne soit pas fourni avec un cadre de tests intégré similaire à ceux disponibles dans des langages comme Python ou JavaScript, vous pouvez toujours mettre en œuvre des procédures de test simples pour vérifier l'intégrité de votre code. Voici un exemple pour illustrer :

Supposons que vous ayez une fonction en VBA qui additionne deux nombres :

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

Pour tester cette fonction, vous pouvez écrire une autre procédure qui valide sa sortie par rapport aux résultats attendus :

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Test réussi !", vbInformation
    Else
        MsgBox "Échec du test. 15 attendu mais obtenu " & result, vbCritical
    End If
End Sub
```

Exécuter `TestAddNumbers` affichera une boîte de message indiquant si le test a réussi ou échoué en fonction de la sortie de la fonction. Bien que ce soit un scénario simplifié, vous pouvez construire des tests plus complexes en incorporant des boucles, différentes valeurs d'entrée, et en testant pour plusieurs fonctions.

## Plongée profonde

L'approche de l'écriture de tests en VBA présentée ici est manuelle et manque des fonctionnalités de cadres de tests plus sophistiqués disponibles dans d'autres environnements de programmation, tels que les exécutions de tests automatisées, les procédures de configuration/nettoyage, et la génération intégrée de rapports de résultats de tests. Avant l'adoption plus large des cadres de tests unitaires et du développement piloté par les tests (TDD), les procédures de tests manuels similaires à celle décrite étaient courantes. Bien que cette méthode soit simple et puisse être efficace pour des petits projets ou à des fins d'apprentissage, elle n'est pas évolutive ou efficace pour des projets plus grands ou des équipes.

Dans des environnements qui soutiennent des ensembles d'outils de développement plus riches, les programmeurs se tournent souvent vers des cadres comme NUnit pour les applications .NET ou JUnit pour les applications Java, qui fournissent des outils complets pour écrire et exécuter des tests de manière systématique. Ces cadres offrent des fonctionnalités avancées telles que l'affirmation des résultats des tests, la configuration d'objets simulés et la mesure de la couverture du code.

Pour les développeurs VBA recherchant des capacités de test plus avancées, l'alternative la plus proche pourrait être l'utilisation d'outils externes ou l'intégration avec d'autres environnements de programmation. Certains développeurs utilisent VBA en conjonction avec Excel pour enregistrer manuellement des scénarios de tests et leurs résultats. Bien que cela ne soit pas aussi pratique ou automatisé qu'utiliser un cadre de tests dédié, ces méthodes peuvent partiellement combler le fossé, aidant à maintenir la fiabilité des solutions VBA dans des applications complexes ou critiques.
