---
title:                "Kotlin: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
L'envoi de requêtes HTTP avec une authentification de base est essentiel pour sécuriser les connexions et limiter l'accès à une API ou un serveur. Cela garantit que seuls les utilisateurs autorisés peuvent accéder à des données sensibles et empêche les intrus de voler des informations confidentielles.

## Comment faire
Il existe plusieurs façons d'envoyer une requête HTTP avec une authentification de base en utilisant Kotlin. Voici un exemple de code qui utilise la bibliothèque Retrofit pour effectuer une requête avec une authentification de base:

```Kotlin
// Importer les dépendances nécessaires
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import okhttp3.Credentials

// Définir l'URL de l'API
val BASE_URL = "https://myapi.com"

// Instancier un objet Retrofit avec le convertisseur Gson et l'URL de l'API
val retrofit = Retrofit.Builder()
    .addConverterFactory(GsonConverterFactory.create())
    .baseUrl(BASE_URL)
    .build()

// Définir l'interface pour les appels à l'API
interface ApiService {
    // Définir la méthode pour l'appel à l'API avec l'authentification de base
    @GET("endpoint")
    fun getData(@Header("Authorization") auth: String): Call<MyData>
}

// Créer une instance de l'interface ApiService en utilisant Retrofit
val service = retrofit.create(ApiService::class.java)

// Générer les informations d'authentification de base à partir du nom d'utilisateur et du mot de passe
val credentials = Credentials.basic("username", "password")

// Effectuer l'appel à l'API pour récupérer les données
service.getData(credentials).enqueue(object: Callback<MyData> {
    override fun onResponse(call: Call<MyData>, response: Response<MyData>) {
        if (response.isSuccessful) {
            // Récupérer les données à partir de la réponse
            val data = response.body()
            // Faire quelque chose avec les données
        }
    }

    override fun onFailure(call: Call<MyData>, t: Throwable) {
        // Gérer les erreurs
    }
})
```

En utilisant cette méthode, nous pouvons facilement envoyer une requête HTTP avec une authentification de base et recevoir une réponse contenant les données demandées.

## Plongée en profondeur
L'utilisation de l'authentification de base pour les requêtes HTTP est considérée comme sécurisée et courante. Cependant, elle peut être quelque peu vulnérable si les informations d'authentification sont interceptées par un intrus. C'est pourquoi il est recommandé d'utiliser également une connexion sécurisée (HTTPS) pour renforcer la sécurité de l'API.

La bibliothèque Retrofit propose également la possibilité de chiffrer les informations d'authentification avec la méthode Digest, qui est plus sécurisée que l'authentification de base standard. Cela peut être fait en utilisant la méthode `Credentials.digest()` à la place de `Credentials.basic()`. Il est également recommandé d'utiliser une connexion sécurisée pour cette méthode d'authentification.

## Voir aussi
- Documentation officielle de Retrofit: https://square.github.io/retrofit/
- Article sur les meilleures pratiques de sécurité pour les API: https://nordicapis.com/best-practices-in-api-security/
- Exemple de mise en œuvre de l'authentification de base avec OkHttp: https://futurestud.io/tutorials/basic-authentication-with-retrofit