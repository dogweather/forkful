---
title:                "C: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Envoyer une requête HTTP est une tâche essentielle pour tout programmeur C travaillant sur des applications Web. Cela permet de communiquer avec des serveurs distants et d'échanger des données, que ce soit pour afficher des informations sur une page ou pour effectuer des actions sur un site.

## Comment faire

Pour envoyer une requête HTTP en utilisant le langage C, il existe différentes bibliothèques telles que "libcurl" ou "libmicrohttpd". Cependant, dans cet article, nous allons utiliser la bibliothèque "libsocket" pour sa simplicité et sa compatibilité avec de nombreux systèmes d'exploitation.

Voici un exemple de code pour envoyer une requête GET en utilisant "libsocket":

```C
#include <stdio.h>
#include <libsocket/libinetsocket.h>

int main() {
    FILE *req = create_inet_request("GET", "https://example.com", 0, NULL);
    send_request(req);
    
    /* Récupère le résultat de la requête */
    char buffer[1024];
    while(receive_line(req, buffer, sizeof(buffer)) > 0) {
        printf("%s\n", buffer);
    }
    destroy_socket_request(req);
    
    return 0;
}
```

Ceci est un exemple simple, mais vous pouvez également personnaliser le type de requête, les entêtes et le corps en utilisant différentes fonctions de la bibliothèque.

Lors de l'exécution, vous devriez obtenir la réponse du serveur affichée sur votre terminal.

## Plongée en profondeur

Lorsque vous envoyez une requête HTTP, il y a plusieurs étapes impliquées. Tout d'abord, le client (votre programme en C) se connecte au serveur à l'aide d'un socket TCP/IP. Ensuite, le client envoie la requête avec la méthode, le chemin et les entêtes spécifiés. Le serveur reçoit la requête et envoie une réponse avec un code d'état et une réponse éventuelle. Le client peut ensuite traiter la réponse et fermer la connexion.

Lors du choix d'une bibliothèque pour envoyer des requêtes HTTP en C, il est important de prendre en compte la compatibilité avec différents systèmes d'exploitation, la simplicité d'utilisation et la documentation fournie.

## Voir aussi

- [Documentation de la bibliothèque libsocket](https://github.com/dermesser/libsocket/blob/master/doc/REQUETES.md)
- [Guide de lib curl pour les requêtes HTTP en C](https://curl.haxx.se/libcurl/c/http.html)
- [Exemple de code pour l'envoi de requêtes HTTP avec libmicrohttpd](https://www.gnu.org/software/libmicrohttpd/tutorial.html#Hello,-World!)