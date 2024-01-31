---
title:                "Envoi d'une requête HTTP"
date:                  2024-01-20T17:59:01.586351-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

Envoyer une requête HTTP, c'est comme passer un coup de fil au serveur web pour demander des données ou soumettre des informations. Les programmeurs le font pour communiquer avec des services web et échanger des données.

## How to: (Comment faire :)

Pour envoyer une requête HTTP en C, on va utiliser la bibliothèque libcurl, simple et puissante.

```c
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // Exécution de la requête
        res = curl_easy_perform(curl);

        // Vérification de l'erreur
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        // Nettoyage
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Le programme ci-dessus envoie une requête GET à example.com. Pas de chichi ; ça marche. 

## Deep Dive (Plongée en Profondeur)

Libcurl est là depuis 1997, offrant une interface multiplateforme pour les coms réseau. Pourquoi pas libhttp ou autre chose ? Libcurl est robuste, supporte plein de protocoles et s'intègre facilement avec le C.

Alternatives ? Il y a des tonnes. PycURL pour Python, OkHttp pour Java... Mais en C, hormis écrire tout à la main (pas vraiment le fun), libcurl est le choix de facto.

Sous le capot, libcurl peut utiliser une variété de transports et s'interface avec les couches sous-jacentes (comme OpenSSL pour HTTPS) pour les détails cryptographiques et de sécurité.

## See Also (Voir Aussi)

Pour creuser plus:

- [libcurl tutorial](https://curl.haxx.se/libcurl/c/libcurl-tutorial.html) - Le tuto officiel pour bien démarrer.
- [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/) - Un guide sur le protocole HTTP, utile pour comprendre ce qui est envoyé et reçu.
- [Stack Overflow](https://stackoverflow.com/) - Des questions ? Le tag 'libcurl' et son cousin 'cURL' sont tes amis.
