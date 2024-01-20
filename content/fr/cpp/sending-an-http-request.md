---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# L'envoi de requêtes HTTP en C++

## Qu'est-ce que c'est et pourquoi ?

L'envoi d'une requête HTTP est un moyen pour votre programme de communiquer avec un serveur web. C'est utile pour accéder à des données web ou interagir avec des API.

## Comment faire:

Pour envoyer une requête HTTP en C++, nous avons besoin d'une bibliothèque comme `cpr`. Vous pouvez l'installer via vcpkg :
```C++
vcpkg install cpr
```
Ensuite, un exemple de requête GET peut être le suivant :
```C++
#include <cpr/cpr.h>
int main(){
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    return 0;
}
```
Affichage de la sortie:
```C++
std::cout << r.status_code << std::endl;                   // 200
std::cout << r.header["content-type"] << std::endl;        // application/json
std::cout << r.text << std::endl;                          // JSON text...
```

## Plongée en profondeur

L'envoi de requêtes HTTP est central en programmation web depuis la naissance du Web en 1989. En C++, avant `cpr`, des bibliothèques comme `libcurl` étaient couramment utilisées, bien qu'elles soient un peu plus complexes à manipuler.

D'autres alternatives incluent `Poco` et `Boost`, mais `cpr` est idéal pour les débutants en raison de son API simple et propre. L'implémentation dépend de libcurl, mais avec une interface plus agréable à utiliser.

Lorsque vous envoyez une requête HTTP, votre programme crée une connexion TCP avec le serveur, envoie des données HTTP formatées spécifiquement, puis attend une réponse.

## Voir aussi 

Pour plus d'informations, consultez les liens suivants :
- CPR GitHub : https://github.com/whoshuu/cpr
- Documentation CPR : https://whoshuu.github.io/cpr/
- Requêtes HTTP Wikipédia : https://fr.wikipedia.org/wiki/Hypertext_Transfer_Protocol