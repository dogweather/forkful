---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyyntö lähettäminen on prosessi, jolla verkkosovellus kommunikoi toisen palvelimen kanssa. Koodarit tekevät näin hakeakseen tai lähettääkseen tietoja.

## Kuinka:

Clojurella voit lähettää HTTP-pyyntöjä `clj-http` -kirjaston avulla. Aloitetaan asentamalla se Leiningen tai Boot avulla:

```Clojure
[clj-http "3.12.0"]
```

Tämän jälkeen voimme lähettää GET-pyynnön seuraavasti:

```Clojure
(ns my-app.main
  (:require [clj-http.client :as client]))

(defn get-request []
  (client/get "http://example.com"))
```

Laajemmin, POST-pyynnön kanssa:

```Clojure
(defn post-request []
  (client/post "http://example.com" {:body "test data"}))
```

Output saattaa näyttää tältä:

```Clojure
{:status 200
 :headers {"content-type" "text/html"}
 :body "<html>..."}
```

## Syvä Sukellus:

Clojuren HTTP-pyynnön lähettäminen on peräisin alhaisen tason TCP/IP-protokollasta. Se on nykyään välttämätöntä kaikille verkkosovelluksille.

On olemassa myös muita kirjastoja, kuten `http-kit` ja Apache HttpClient, mutta `clj-http` tarjoaa parasta tukea ja helppokäyttöisyyttä Clojure-kehittäjille.

HTTP-pyynnön lähettämisen toteutus pitää sisällään lukuisia yksityiskohtia. Se käsittää mm. protokollan päättelyn, lähettäjän ja vastaanottajan IP-osoitteiden selvittämisen sekä itse datan lähettämisen.

## Katso Myös:

- clj-http GitHub Repo: https://github.com/dakrone/clj-http
- http-kit GitHub Repo: https://github.com/http-kit/http-kit
- Apache HttpClient: https://hc.apache.org/httpcomponents-client-ga/