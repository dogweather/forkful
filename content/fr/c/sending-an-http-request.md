---
title:                "Envoyer une requête http"
html_title:           "C: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs le font?

L'envoi d'une requête HTTP est essentiellement un moyen pour un programme informatique de communiquer avec un serveur. Cela peut se faire en utilisant des protocoles de communication tels que HTTP, HTTPS, FTP, etc. Les programmeurs utilisent cette méthode pour accéder aux ressources en ligne telles que les sites Web, les API et les bases de données.

# Comment faire:

```c
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>
  
int main(void)
{
  CURL *curl;
  CURLcode res;
  
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    res = curl_easy_perform(curl);
    // gérer le code de réponse ici
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
**Sortie:**

Si la requête est un succès, vous obtiendrez une réponse avec un code de statut HTTP de 200 OK et les données demandées seront renvoyées. Si la requête échoue, vous recevrez un code d'erreur et devrez gérer l'erreur en conséquence.

# Plongez plus en profondeur:

**Contexte historique:**

L'utilisation de requêtes HTTP a commencé avec le développement du World Wide Web dans les années 1990. Au départ, il s'agissait principalement d'une méthode pour récupérer des documents HTML, mais avec l'évolution de la technologie, il est devenu un moyen important pour échanger des données entre clients et serveurs.

**Alternatives:**

Bien qu'HTTP soit le protocole de communication le plus couramment utilisé pour les requêtes, il existe d'autres options telles que HTTPS pour une communication sécurisée, FTP pour le transfert de fichiers et P2P pour le partage de fichiers pair à pair.

**Détails d'implémentation:**

L'implémentation de requêtes HTTP peut se faire en utilisant des bibliothèques telles que libcurl, libmicrohttpd, etc. Les serveurs web et les frameworks tels que Node.js peuvent également être utilisés pour traiter les requêtes HTTP.

# Voir aussi:

Pour en savoir plus sur les requêtes HTTP et comment les utiliser dans vos projets, consultez les ressources suivantes:

- [Site officiel de Curl](https://curl.se)
- [Documentation sur les requêtes HTTP en C](https://curl.se/libcurl/c/http.html)
- [Livre «HTTP Pocket Reference» de Clinton Wong](https://www.oreilly.com/library/view/http-pocket-reference/9781492043801/)