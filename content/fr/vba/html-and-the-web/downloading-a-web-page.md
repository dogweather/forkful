---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:41.643128-07:00
description: "T\xE9l\xE9charger une page web en Visual Basic pour Applications (VBA)\
  \ consiste \xE0 r\xE9cup\xE9rer le contenu HTML d'une page web depuis Internet.\
  \ Les programmeurs\u2026"
lastmod: 2024-02-19 22:05:16.353865
model: gpt-4-0125-preview
summary: "T\xE9l\xE9charger une page web en Visual Basic pour Applications (VBA) consiste\
  \ \xE0 r\xE9cup\xE9rer le contenu HTML d'une page web depuis Internet. Les programmeurs\u2026"
title: "T\xE9l\xE9charger une page web"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Télécharger une page web en Visual Basic pour Applications (VBA) consiste à récupérer le contenu HTML d'une page web depuis Internet. Les programmeurs effectuent souvent cette tâche pour traiter ou analyser le contenu des sites web de manière programmatique, depuis Excel, Access ou d'autres applications Office.

## Comment faire :

Pour télécharger une page web en VBA, vous pouvez utiliser la bibliothèque Microsoft XML, v6.0 (MSXML6), qui permet les requêtes HTTP serveur. Avant de plonger dans le code, assurez-vous d'avoir activé cette référence dans votre éditeur VBA en allant dans `Outils` -> `Références` et en cochant `Microsoft XML, v6.0`.

Voici un exemple simple de la manière de télécharger le contenu HTML d'une page web :

```basic
Sub DownloadWebPage()
    Dim request As Object
    Dim url As String
    Dim response As String
    
    ' Initialisation de l'objet de requête XML HTTP
    Set request = CreateObject("MSXML2.XMLHTTP")
    
    url = "http://www.example.com"
    
    ' Ouverture d'une requête synchrone
    request.Open "GET", url, False
    
    ' Envoi de la requête au serveur
    request.send
    
    ' Obtention du texte de réponse
    response = request.responseText
    
    ' Affichage de la réponse dans la fenêtre immédiate (à des fins de débogage)
    Debug.Print response
    
    ' Nettoyage
    Set request = Nothing
End Sub
```

L'exécution de cette sous-routine affichera le HTML de `http://www.example.com` dans la Fenêtre immédiate de l'éditeur VBA. Notez que le paramètre `False` dans la méthode `Open` rend la requête synchrone, ce qui signifie que le code attendra que la page web soit téléchargée avant de passer à la ligne suivante.

## Exploration approfondie

La technique présentée repose sur MSXML, l'implémentation par Microsoft de la norme XML HTTP Request, souvent utilisée pour les requêtes AJAX en développement web. Ce composant fait partie de la pile technologique de Microsoft depuis longtemps, ce qui en fait un choix robuste pour les requêtes réseau en VBA.

Cependant, la dépendance à MSXML et VBA pour télécharger et analyser le contenu web peut être limitante, en particulier avec les applications web modernes qui utilisent fortement JavaScript pour le rendu de contenu dynamique. Ces limitations peuvent rendre d'autres langages ou outils comme Python avec des bibliothèques telles que BeautifulSoup ou Selenium plus adaptés aux tâches de scraping web en raison de leur capacité à exécuter JavaScript et à gérer des interactions de sites web complexes.

Malgré cela, pour des tâches simples impliquant la récupération de contenu HTML simple ou lorsqu'on travaille dans le cadre des applications Office, VBA reste un outil pratique. Son intégration au sein de la suite Office permet une manipulation directe des documents basée sur le contenu web, offrant un avantage unique pour des cas d'utilisation spécifiques.
