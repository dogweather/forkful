---
title:                "Envoi d'une demande http avec une authentification de base"
html_title:           "Kotlin: Envoi d'une demande http avec une authentification de base"
simple_title:         "Envoi d'une demande http avec une authentification de base"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Les requêtes HTTP sont un moyen couramment utilisé pour communiquer avec des serveurs à travers le web. L’utilisation d’une authentification basique permet de sécuriser ces requêtes en vérifiant l’identité de l’utilisateur. 

## Comment faire

Pour envoyer une requête HTTP avec une authentification basique en utilisant Kotlin, vous pouvez suivre les étapes suivantes :

1. Importer la librairie HTTP dans votre code en utilisant `import com.android.volley.toolbox.HttpClient`
2. Créer une instance d’HttpClient  pour effectuer la requête 
3. Utiliser la méthode `setCredentials()` pour ajouter les informations d’authentification de base dans l’en-tête de la requête

```
import com.android.volley.toolbox.HttpClient
val httpClient = HttpClient()
httpClient.setCredentials(username, password)
```

4. Définir l’URL de la requête à l’aide de la méthode `setUrl()`
5. Utiliser la méthode `setMethod()` pour spécifier le type de requête (GET, POST, etc.)
6. Enfin, utiliser la méthode `executeRequest()` pour envoyer la requête et récupérer la réponse.

```
httpClient.setUrl("https://monsite.com/api")
httpClient.setMethod("GET")
val response = httpClient.executeRequest()
println(response)
```

Les informations d’authentification fournies seront incluses dans l’en-tête de la requête envoyée.

## Plongée en profondeur

L’authentification basique est un moyen simple mais efficace de sécuriser les requêtes HTTP. Elle utilise un en-tête d’autorisation pour inclure le nom d’utilisateur et le mot de passe de l’utilisateur dans la requête. Le serveur vérifie ensuite ces informations pour valider ou refuser la demande.

Il est important de garder à l’esprit que l’authentification basique n’est pas sécurisée par rapport à d’autres méthodes. Les informations d’identification sont envoyées en texte clair, donc elles peuvent être facilement interceptées si elles sont envoyées sur un réseau non sécurisé. Il est également recommandé d’utiliser l’authentification à deux facteurs pour une sécurité accrue.

## Voir aussi

- [Documentation officielle pour l’utilisation de HttpClient en Kotlin](https://developer.android.com/training/volley/request-custom#kotlin)
- [Guide complet pour l’authentification basique en utilisant Kotlin](https://www.novoda.com/blog/https-authentication-with-android-volley/)