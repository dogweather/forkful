---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:23.656253-07:00
description: "Envoyer une requ\xEAte HTTP avec authentification de base dans Visual\
  \ Basic pour Applications (VBA) consiste \xE0 acc\xE9der aux ressources web prot\xE9\
  g\xE9es par des\u2026"
lastmod: '2024-03-11T00:14:31.544556-06:00'
model: gpt-4-0125-preview
summary: "Envoyer une requ\xEAte HTTP avec authentification de base dans Visual Basic\
  \ pour Applications (VBA) consiste \xE0 acc\xE9der aux ressources web prot\xE9g\xE9\
  es par des\u2026"
title: "Envoyer une requ\xEAte HTTP avec une authentification de base"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Envoyer une requête HTTP avec authentification de base dans Visual Basic pour Applications (VBA) consiste à accéder aux ressources web protégées par des identifiants de nom d'utilisateur et mot de passe. Les programmeurs font cela pour interagir avec des API sécurisées ou des services web au sein de leurs applications alimentées par VBA, comme automatiser des tâches dans Excel ou Access avec des données provenant de points de terminaison sécurisés.

## Comment faire :

Dans VBA, vous pouvez utiliser la bibliothèque `Microsoft XML, v6.0` (MSXML2) pour envoyer des requêtes HTTP avec authentification de base. Cela implique de définir l'en-tête `"Authorization"` de la requête pour inclure les identifiants dans un format codé en base64. Voici un guide étape par étape :

1. **Référencer MSXML2** : D'abord, assurez-vous que votre projet VBA fait référence à la bibliothèque `Microsoft XML, v6.0`. Dans l'éditeur VBA, allez dans Outils > Références et cochez `Microsoft XML, v6.0`.

2. **Créer et envoyer la requête HTTP** : Utilisez le morceau de code VBA suivant comme guide. Remplacez `"votre_nom_utilisateur"` et `"votre_mot_de_passe"` par vos véritables identifiants et ajustez l'URL selon le besoin.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Remplacez par l'URL réelle
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("votre_nom_utilisateur:votre_mot_de_passe")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Affiche la réponse dans la fenêtre Immédiate
    ```

3. **Encoder les identifiants en base64** : VBA n'a pas de fonction intégrée pour le codage en base64, mais vous pouvez utiliser cette fonction personnalisée `EncodeBase64` :

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
Cela enverra une requête GET à `http://example.com/api/resource` avec les identifiants d'authentification de base spécifiés, et imprimera la réponse.

## Plongée Profonde

L'approche utilisée ici, bien qu'efficace pour des cas d'utilisation simples, repose sur le schéma d'Authentification de Base, qui envoie les identifiants dans un format facilement décodable (le codage en base64 n'est pas un chiffrement). En raison de sa vulnérabilité, en particulier dans des contextes non-HTTPS, l'Authentification de Base n'est pas recommandée pour la transmission de données sensibles sur Internet sans couches de sécurité supplémentaires comme SSL/TLS.

Historiquement, l'Authentification de Base était l'une des premières méthodes développées pour contrôler l'accès aux ressources web. Aujourd'hui, des normes d'authentification plus sûres et plus flexibles, telles que OAuth 2.0, sont généralement préférées pour les nouvelles applications. Étant donné les limitations de VBA et les dépendances externes requises pour des méthodes d'authentification plus avancées, les développeurs utilisent souvent VBA dans des environnements internes ou moins critiques en termes de sécurité, ou l'utilisent comme un tremplin pour prototyper rapidement des idées.

Lors de l'utilisation de VBA pour des requêtes HTTP, n'oubliez pas que chaque version de la bibliothèque MSXML peut supporter différentes fonctionnalités et normes de sécurité. Utilisez toujours la version la plus récente compatible avec votre application pour garantir une meilleure sécurité et performance. De plus, considérez les limitations environnementales et les fonctionnalités potentiellement obsolètes lors du choix de VBA pour de nouveaux projets, en particulier ceux nécessitant des communications HTTP sécurisées. D'autres environnements de programmation ou langages pourraient offrir des solutions plus robustes, sécurisées et maintenables pour des tâches similaires.
