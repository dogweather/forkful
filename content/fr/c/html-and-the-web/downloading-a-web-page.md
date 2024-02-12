---
title:                "Téléchargement d'une page web"
aliases:
- /fr/c/downloading-a-web-page.md
date:                  2024-02-03T17:55:43.813371-07:00
model:                 gpt-4-0125-preview
simple_title:         "Téléchargement d'une page web"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Télécharger une page web en C implique d'accéder programmatiquement au contenu d'une page web sur Internet et de le sauvegarder localement pour un traitement ou une utilisation hors ligne. Les programmeurs s'engagent souvent dans cette activité pour consommer des services web, extraire du contenu web, ou interagir directement avec des ressources en ligne depuis leurs applications.

## Comment faire :

Pour télécharger une page web en C, une approche populaire consiste à utiliser la bibliothèque libcurl, une bibliothèque de transfert d'URL côté client efficace et portable. Assurez-vous d'avoir libcurl installé et lié dans votre projet. Voici un exemple illustrant comment utiliser libcurl pour télécharger le contenu d'une page web :

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Initialiser une session libcurl facile
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback pour écrire les données reçues
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Définir le pointeur de fichier pour écrire les données

        res = curl_easy_perform(curl); // Effectuer le téléchargement du fichier
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() a échoué : %s\n",
                    curl_easy_strerror(res));
        }

        /* toujours nettoyer */
        curl_easy_cleanup(curl); // Nettoyer la session facile
        fclose(fp); // Fermer le flux de fichier
    }
    return 0;
}
```
Sortie d'échantillon (aucune sortie visible dans la console) : Ce code télécharge le contenu à l'URL spécifiée et le sauvegarde dans un fichier nommé `downloaded_page.html`. Vérifiez le répertoire de votre programme pour ce fichier afin de voir le contenu téléchargé.

## Approfondissement :

Historiquement, télécharger du contenu web en C était plus laborieux, nécessitant une programmation de socket manuelle et une gestion du protocole HTTP. Libcurl abstrait ces complexités, offrant une API robuste et de haut niveau pour le transfert de données sur le web.

Bien que libcurl simplifie les requêtes HTTP en C, les langages de programmation modernes comme Python avec leur bibliothèque `requests` ou JavaScript (Node.js) avec diverses bibliothèques clientes HTTP peuvent offrir une syntaxe plus intuitive et un support intégré pour JSON et d'autres formats de données couramment utilisés dans la communication web. Cependant, C et libcurl fournissent une solution performante et stable pour les systèmes où l'efficacité, le contrôle précis ou l'intégration dans des bases de code C existantes sont critiques. Il convient également de noter que le C, combiné avec libcurl, peut être utilisé pour bien plus que le simple téléchargement de pages web - il est capable de gérer FTP, SMTP, et bien plus encore, ce qui en fait un outil polyvalent dans la trousse à outils d'un programmeur.
