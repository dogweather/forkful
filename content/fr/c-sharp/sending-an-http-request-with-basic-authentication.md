---
title:                "Envoi d'une requête http avec une authentification de base"
html_title:           "C#: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous travaillez sur une application ou un site web qui communique avec un serveur externe, il est possible que vous ayez besoin d'envoyer une requête HTTP avec une authentification de base (basic authentication). Cette méthode d'authentification est couramment utilisée pour sécuriser les informations échangées entre un client et un serveur.

# Comment faire

Voici un exemple de code en C# pour envoyer une requête HTTP avec une authentification de base :

```C#
public static void SendRequestWithBasicAuth(string url, string username, string password)
{
    try
    {
        // Création de la requête HTTP
        var request = (HttpWebRequest)WebRequest.Create(url);

        // Ajout des informations d'authentification dans l'en-tête de la requête
        string authInfo = username + ":" + password;
        authInfo = Convert.ToBase64String(Encoding.Default.GetBytes(authInfo));
        request.Headers.Add("Authorization", "Basic " + authInfo);

        // Spécification de la méthode de la requête (dans ce cas, GET)
        request.Method = "GET";

        // Récupération de la réponse du serveur
        var response = (HttpWebResponse)request.GetResponse();

        // Lecture de la réponse
        string content = new StreamReader(response.GetResponseStream()).ReadToEnd();

        // Affichage du résultat
        Console.WriteLine(content);
    }
    catch (Exception e)
    {
        Console.WriteLine(e.Message);
    }
}
```

En appelant cette fonction avec l'URL souhaitée, le nom d'utilisateur et le mot de passe, vous allez envoyer une requête HTTP avec une authentification de base et recevoir la réponse du serveur.

# Plongée en profondeur

L'authentification de base est un moyen simple de sécuriser l'accès à une ressource. Lorsque le client envoie une requête avec une authentification de base, il ajoute les informations d'authentification dans l'en-tête de la requête en utilisant le schema "Basic". Ensuite, le serveur va vérifier ces informations et renvoyer la réponse appropriée.

Il est important de noter que cette méthode d'authentification n'est pas considérée comme sécurisée car les informations d'identification sont envoyées en clair dans l'en-tête de la requête. Il est donc recommandé d'utiliser d'autres méthodes d'authentification plus sécurisées si possible.

# Voir aussi

- [Documentation Microsoft sur les requêtes HTTP en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.net.httpwebrequest?view=net-5.0)
- [Article sur la sécurisation des requêtes HTTP en C#](https://www.codeproject.com/Articles/1217149/Basic-Authentication-with-HttpClient)