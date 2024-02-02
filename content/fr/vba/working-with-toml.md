---
title:                "Travailler avec TOML"
date:                  2024-02-01T22:05:55.705180-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/vba/working-with-toml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

TOML, qui signifie "Tom's Obvious, Minimal Language" (le langage minimal et évident de Tom), est un format de sérialisation de données principalement utilisé pour les fichiers de configuration. Les programmeurs exploitent TOML pour sa lisibilité et son mappage facile aux structures de données, permettant une configuration simple des applications à travers divers environnements de programmation, y compris Visual Basic pour Applications (VBA).

## Comment faire :

Travailler avec TOML dans VBA implique d'analyser le fichier TOML pour lire les configurations ou les paramètres dans votre projet VBA. VBA n'offre pas de support intégré pour TOML, donc vous utiliserez typiquement un analyseur ou convertirez les données TOML en un format avec lequel VBA peut facilement travailler, comme JSON ou XML. Voici comment analyser manuellement un fichier de configuration TOML simple :

1. **Fichier TOML d'Exemple** (`config.toml`):
```
titre = "Exemple TOML"

[base_de_donnees]
serveur = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connexion_max = 5000
active = vrai
```

2. **Code VBA Pour Analyser TOML** :

En supposant que le contenu TOML est lu dans une variable de chaîne `tomlStr`, le code VBA suivant démontre une approche simpliste pour analyser la section `[base_de_donnees]`:

```vb
Function AnalyserTOML(tomlStr As String)
    Dim lignes() As String
    lignes = Split(tomlStr, vbCrLf)
    
    Dim config As Object
    Set config = CreateObject("Scripting.Dictionary")
    Dim sectionCourante As String
    sectionCourante = ""
    
    Dim i As Integer
    For i = 0 To UBound(lignes)
        Dim ligne As String
        ligne = Trim(lignes(i))
        If InStr(ligne, "[") > 0 And InStr(ligne, "]") > 0 Then
            sectionCourante = Mid(ligne, 2, Len(ligne) - 2)
            Set config(sectionCourante) = CreateObject("Scripting.Dictionary")
        ElseIf InStr(ligne, "=") > 0 Then
            Dim parties() As String
            parties = Split(ligne, "=")
            Dim cle As String
            cle = Trim(parties(0))
            Dim valeur As String
            valeur = Trim(parties(1))
            config(sectionCourante)(cle) = valeur
        End If
    Next i
    
    'Exemple pour accéder aux données analysées
    Debug.Print "Serveur Base de Données : "; config("base_de_donnees")("serveur")
End Function
```

3. **Exemple de Sortie** (Fenêtre Immédiate) :
```
Serveur Base de Données : 192.168.1.1
```

## Exploration Plus Approfondie

L'acceptation pratique de TOML dans la communauté des développeurs montre une tendance vers des fichiers de configuration plus simples et plus lisibles par l'humain, contrastant avec le XML autrefois prévalent. La philosophie de conception de TOML met l'accent sur des sémantiques claires et vise une analyse facile avec un minimum de surcharge. Dans VBA, le traitement direct de TOML implique une analyse manuelle ou l'utilisation d'outils externes pour convertir TOML en un format plus adapté à VBA en raison de l'absence de support natif. Bien que cette méthode d'analyse manuelle présente une approche fondamentale, l'utilisation de bibliothèques externes ou de formats intermédiaires comme JSON peut offrir des stratégies d'analyse plus robustes et moins sujettes à erreur. Étant donné l'intégration étendue de VBA avec Microsoft Office, convertir TOML en JSON et utiliser les capacités d'analyse JSON natives de VBA (lorsqu'applicables) ou des analyseurs JSON tiers pourrait fournir un flux de travail plus rationalisé. De plus, avec l'évolution continue des formats de sérialisation des données, les programmeurs devraient également considérer YAML, qui, comme TOML, met l'accent sur la lisibilité par l'humain mais offre différents compromis en termes de complexité et de flexibilité.
