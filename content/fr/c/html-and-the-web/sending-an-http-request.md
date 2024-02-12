---
title:                "Envoyer une requête HTTP"
aliases:
- /fr/c/sending-an-http-request.md
date:                  2024-02-03T18:08:30.694945-07:00
model:                 gpt-4-0125-preview
simple_title:         "Envoyer une requête HTTP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Envoyer une requête HTTP consiste à créer et envoyer une requête à un serveur web pour récupérer ou soumettre des données. Les programmeurs font cela en C pour interagir avec les API web, télécharger des pages web ou communiquer directement avec d'autres services en réseau depuis leurs applications.

## Comment :

Pour envoyer une requête HTTP en C, vous vous appuierez généralement sur des bibliothèques comme libcurl, car le C n'intègre pas de support natif pour les protocoles web. Voici un exemple simple utilisant libcurl pour effectuer une requête GET :

D'abord, assurez-vous d'avoir libcurl installé sur votre système. Ensuite, incluez les en-têtes nécessaires et liez-les contre la bibliothèque libcurl dans votre fichier source :

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init(); // Initialise une poignée libcurl
    if(curl) {
        // Définit l'URL qui reçoit la poignée libcurl
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // Définit une fonction de rappel pour obtenir les données
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); 
        
        // Exécute la requête, res recevra le code de retour
        res = curl_easy_perform(curl);
        // Vérifier les erreurs
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() a échoué : %s\n",
                    curl_easy_strerror(res));

        // Nettoyage systématique 
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Compilez ceci avec une commande semblable à `gcc -o http_request http_request.c -lcurl`, l'exécution devrait réaliser une simple requête GET vers "http://example.com".

### Exemple de sortie

Comme l'exemple ne traite pas la réponse du serveur, son exécution ne produira pas de sortie visible au-delà des éventuels messages d'erreur. Intégrer la fonction de rappel pour le traitement des données reçues est essentiel pour une interaction significative.

## Approfondissement

Le concept d'envoi de requêtes HTTP depuis un programme C repose sur les puissantes capacités de mise en réseau du langage, associées à des bibliothèques externes puisque le C lui-même est un langage de bas niveau sans support intégré pour les protocoles internet de haut niveau. Historiquement, les programmeurs utilisaient manuellement la programmation de sockets en C, un processus complexe et fastidieux, pour interagir avec les serveurs web avant l'avènement de bibliothèques dédiées comme libcurl.

Libcurl, construit sur le C, simplifie le processus, en masquant les détails ardus de la programmation de sockets et les spécificités du protocole HTTP. Il prend en charge une multitude de protocoles au-delà du HTTP/HTTPS, y compris FTP, SMTP, et plus, le rendant un outil polyvalent pour la programmation réseau en C.

Bien que l'utilisation de libcurl pour les requêtes HTTP en C soit pratique, la programmation moderne tend souvent vers des langages intégrant nativement ce type de tâches, comme Python (bibliothèque requests) ou JavaScript (API Fetch). Ces alternatives offrent une syntaxe plus simple, plus lisible au détriment du contrôle granulaire et des optimisations de performance possibles en C par manipulation directe des sockets et utilisation minutieuse des bibliothèques.

Pour les applications critiques en termes de performance ou lorsque une interaction directe au niveau du système est nécessaire, le C reste une option viable, particulièrement avec libcurl qui facilite la complexité de la communication web. Cependant, pour la plupart des interactions web de haut niveau, explorer des langages de programmation web plus dédiés pourrait s'avérer plus efficace.
