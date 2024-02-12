---
title:                "Envoyer une requête HTTP avec une authentification de base"
aliases:
- /fr/c/sending-an-http-request-with-basic-authentication.md
date:                  2024-02-03T18:09:00.344765-07:00
model:                 gpt-4-0125-preview
simple_title:         "Envoyer une requête HTTP avec une authentification de base"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Envoyer une requête HTTP avec une authentification de base en C nécessite de créer une requête HTTP qui inclut un en-tête d'autorisation avec les informations d'identification de l'utilisateur encodées en Base64. C'est une méthode courante pour ajouter une couche d'authentification simple aux requêtes HTTP, permettant d'accéder de manière programmatique à des ressources restreintes.

## Comment faire :
Pour envoyer une requête HTTP avec une authentification de base en C, nous aurons besoin d'utiliser la bibliothèque libcurl, une bibliothèque de transfert d'URL côté client populaire, polyvalente et facile à utiliser. Elle gère divers protocoles, y compris HTTP et HTTPS, ce qui simplifie notre tâche. Assurez-vous que libcurl est installé sur votre système avant de continuer. Voici un exemple basique qui montre comment envoyer une requête GET avec une authentification de base :

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // L'URL vers laquelle la requête est envoyée
        curl_easy_setopt(curl, CURLOPT_URL, "http://exemple.com/ressource");
        // Activation de l'utilisation de l'authentification de base
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Fourniture du nom d'utilisateur et du mot de passe pour l'authentification de base
        curl_easy_setopt(curl, CURLOPT_USERPWD, "nomutilisateur:motdepasse");

        // Exécution de la requête GET
        res = curl_easy_perform(curl);

        // Vérification des erreurs
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() a échoué : %s\n",
                    curl_easy_strerror(res));

        // Nettoyage systématique
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
Dans l'exemple ci-dessus, remplacez `"http://exemple.com/ressource"`, `"nomutilisateur"`, et `"motdepasse"` par votre URL, nom d'utilisateur, et mot de passe réels.

Ce code initialise un objet `CURL`, définit l'URL, active l'authentification HTTP de base, et spécifie les identifiants. Il envoie ensuite la requête et se nettoie après lui-même. Si réussi, la ressource demandée est récupérée ; s'il y a une erreur, elle est imprimée sur stderr.

La sortie d'exemple (en supposant une authentification réussie et un accès à la ressource) pourrait ne pas être directement montrée par le programme, car l'exemple montre principalement l'envoi de la requête. Pour imprimer la réponse, vous devriez étendre le programme pour gérer les données de réponse HTTP.

## Plongée profonde :
Envoyer des requêtes HTTP avec une authentification de base en C, comme montré, tire parti de la bibliothèque libcurl pour sa robustesse et sa simplicité. Historiquement, créer des requêtes HTTP purement en C sans de telles bibliothèques était fastidieux et sujet aux erreurs, impliquant une programmation de socket de bas niveau et la construction manuelle des en-têtes HTTP.

L'authentification de base elle-même est une méthode des premiers jours du web. Elle envoie les informations d'identification dans un format facilement décodable (Base64), qui est intrinsèquement peu sûr sur les canaux en texte clair. Les applications modernes préfèrent souvent des méthodes d'authentification plus sécurisées, comme OAuth 2.0 ou JWT (Jetons Web JSON), surtout pour les données sensibles.

Cependant, pour les systèmes internes, moins critiques, ou les scripts rapides et sales où la commodité l'emporte sur les préoccupations de sécurité, l'authentification de base reste utilisée. De plus, lorsqu'elle est combinée avec des connexions chiffrées (HTTPS), sa simplicité devient un avantage pour le développement rapide, les tests, ou le travail d'automatisation où des mécanismes de sécurité de niveau supérieur ne sont pas aussi nécessaires.

Dans les contextes où la sécurité de pointe est non négociable, des alternatives comme l'authentification basée sur des jetons devraient être privilégiées. Néanmoins, comprendre comment implémenter l'authentification de base en C via libcurl fournit une compétence fondamentale qui peut être adaptée à diverses méthodes d'authentification et protocoles, reflétant les compromis nuancés entre sécurité, commodité et exigences d'application dans le développement web.
