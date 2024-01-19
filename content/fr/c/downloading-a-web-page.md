---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Télécharger une page web, c'est essentiellement copier son contenu depuis le serveur web vers un ordinateur local. Les programmeurs font cela pour extraires des données, tester la disponibilité des serveurs, ou sauvegarder du contenu pour une utilisation hors ligne.

## Comment faire:
L'exemple de code suivant en C illustre comment utiliser la librairie `curl` pour télécharger une page web.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    res = curl_easy_perform(curl);
    
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() a échoué: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();
  return 0;
}
```
Sortie d'échantillon :

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Approfondissement
Télécharger une page web est une pratique qui existe depuis les débuts du web. Pendant longtemps, `wget` et `curl` ont été les outils privilégiés pour cette tâche. Il existe d'autres méthodes, comme l'utilisation de librairies spécifiques à certains langages de programmation. L'implémentation varie avec le langage, mais la philosophie reste la même : envoyer une requête HTTP au serveur et enregistrer la réponse.

## Voir Aussi
- Documentation officielle libcurl : https://curl.haxx.se/libcurl/
- UNIX man page pour curl : https://man7.org/linux/man-pages/man1/curl.1.html
- Guide de programmation avec libcurl : https://curl.se/libcurl/c/libcurl-tutorial.html