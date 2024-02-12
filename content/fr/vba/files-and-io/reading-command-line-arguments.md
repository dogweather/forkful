---
title:                "Lecture des arguments de ligne de commande"
aliases:
- /fr/vba/reading-command-line-arguments.md
date:                  2024-02-01T21:59:03.768832-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lecture des arguments de ligne de commande"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/vba/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

La lecture des arguments de la ligne de commande dans Visual Basic pour Applications (VBA) implique l'accès aux paramètres passés à votre programme lors de son exécution. Cette technique est souvent utilisée pour influencer le comportement ou la sortie d'un programme sans nécessiter d'interaction de l'utilisateur, rendant ainsi les tâches d'automatisation et de script significativement plus simples et polyvalentes.

## Comment faire :

Contrairement à des environnements de programmation plus simples, VBA n'a pas de fonction intégrée pour lire directement les arguments de la ligne de commande dans un sens conventionnel parce qu'il est principalement conçu pour être intégré dans les applications Microsoft Office. Cependant, avec un peu de créativité, nous pouvons utiliser le Windows Script Host (WSH) ou appeler des APIs externes pour atteindre une fonctionnalité similaire. Voici une solution pratique en utilisant WSH :

1. **Créer un VBScript pour passer les arguments à VBA :**

   Tout d'abord, écrivez un fichier VBScript (*yourScript.vbs*) qui lance votre application VBA (par exemple, une macro Excel) et passe les arguments de la ligne de commande :

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\VotreClasseurDeMacro.xlsm"
objExcel.Run "VotreNomDeMacro", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Accéder aux arguments dans VBA :**

   Dans votre application VBA (*VotreClasseurDeMacro.xlsm*), modifiez ou créez la macro (*VotreNomDeMacro*) pour accepter des paramètres :

```vb
Sub VotreNomDeMacro(arg1 As String, arg2 As String)
    MsgBox "Argument 1 : " & arg1 & " Argument 2 : " & arg2
End Sub
```

3. **Exécuter votre script :**

   Executez le VBScript depuis la ligne de commande, en passant les arguments selon le besoin :

```shell
cscript yourScript.vbs "Bonjour" "Monde"
```

   Cela devrait aboutir à l'exécution de votre macro VBA avec les arguments "Bonjour" et "Monde", les affichant dans une boîte de message.

## Approfondissement :

Dans son contexte historique, VBA a été conçu pour étendre les capacités des applications Microsoft Office, et non comme un environnement de programmation autonome. Par conséquent, l'interaction directe avec la ligne de commande est en dehors de son champ d'application principal, ce qui explique le manque de support intégré pour la lecture des arguments de la ligne de commande.

La méthode décrite ci-dessus, bien qu'efficace, est plus une solution de contournement qu'une solution native, exploitant un script externe pour combler le fossé. Cette approche peut introduire de la complexité et des préoccupations de sécurité potentielles car elle nécessite l'activation des macros et potentiellement l'abaissement des paramètres de sécurité pour exécuter.

Pour les tâches fortement dépendantes des arguments de la ligne de commande ou nécessitant une intégration plus transparente avec le système d'exploitation Windows, d'autres langages de programmation comme PowerShell ou Python pourraient offrir des solutions plus robustes et sécurisées. Ces alternatives fournissent un support direct pour les arguments de la ligne de commande et sont mieux adaptées pour les applications autonomes ou les scripts nécessitant une entrée externe pour modifier dynamiquement leur comportement.
