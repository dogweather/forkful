---
title:                "Telechargement d'une page web"
html_title:           "C++: Telechargement d'une page web"
simple_title:         "Telechargement d'une page web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur web ou que vous vous intéressez à la programmation, vous savez probablement que le téléchargement d'une page web peut être une tâche utile et importante. Que vous souhaitiez récupérer des informations à des fins de traitement ou simplement sauvegarder une page pour une utilisation ultérieure, savoir comment télécharger une page web est une compétence précieuse à avoir.

## Comment Faire

Le téléchargement d'une page web en C++ peut sembler compliqué, mais avec les bonnes techniques, cela peut être assez simple. La première étape consiste à inclure la bibliothèque " ```<iostream>``` " pour gérer les flux d'entrée et de sortie. Ensuite, vous devrez également inclure la bibliothèque "```<curl/curl.h>```" qui vous permettra d'établir une connexion avec le serveur web et de télécharger la page. Voici un exemple de code qui utilise ces bibliothèques :

```
#include <iostream>
#include <curl/curl.h>

int main()
{
  CURL* curl; //Déclaration de l'objet Curl
  CURLcode res; //Déclaration de la variable pour stocker le code de réponse

  //Initialisation de l'objet Curl
  curl = curl_easy_init();
  
  //Vérification des erreurs lors de l'initialisation
  if(curl)
  {
    //Définition de l'URL de la page à télécharger
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    
    //Téléchargement de la page
    res = curl_easy_perform(curl);
    
    //Vérification du code de réponse
    if(res != CURLE_OK)
    {
      std::cout << "Erreur lors du téléchargement de la page : " << curl_easy_strerror(res) << std::endl;
    }
    
    //Nettoyage de l'objet Curl
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Ce code utilise la fonction "```curl_easy_init()```" pour initialiser l'objet Curl, puis la fonction "```curl_easy_setopt()```" pour spécifier l'URL de la page à télécharger et enfin la fonction "```curl_easy_perform()```" pour effectuer le téléchargement. Si le téléchargement est réussi, le code de réponse sera "CURLE_OK". Sinon, vous pouvez utiliser la fonction "```curl_easy_strerror()```" pour afficher un message d'erreur.

## Deep Dive

Maintenant que vous savez comment télécharger une page web en utilisant C++, vous pouvez explorer d'autres options et paramètres pour personnaliser votre téléchargement. Par exemple, vous pouvez définir des en-têtes HTTP personnalisés à l'aide de la fonction "```CURLOPT_HTTPHEADER```" ou encore spécifier un délai d'attente à l'aide de la fonction "```CURLOPT_TIMEOUT```". Vous pouvez également utiliser la fonction "```CURLOPT_WRITEFUNCTION```" pour spécifier une fonction de rappel qui sera appelée pour écrire les données téléchargées dans un fichier. En explorant ces options et en expérimentant avec différents paramètres, vous pourrez télécharger des pages web de manière plus efficace et ciblée.

## Voir aussi

- [Documentation officielle de la bibliothèque Curl] (https://curl.haxx.se/libcurl/c/)
- [Tutoriel pour le téléchargement de pages web en C++] (https://www.tutorialspoint.com/download-web-page-using-c-plus-plus)
- [Code source complet pour télécharger une page web en C++] (https://gist.github.com/algorithmist/6a61dfac38c438c982fa)