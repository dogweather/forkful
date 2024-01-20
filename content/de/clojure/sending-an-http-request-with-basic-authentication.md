---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung ermöglicht es einem Programm, Daten von einem serverseitigen Dienst sicher abzurufen. Programmierer verwenden es, um Zugriff auf geschützte Ressourcen, während die Identität des Antragsstellers bestätigt wird.

## So geht's:
In Clojure nutzen wir die Funktion `clj-http.client/get` aus dem clj-http Package, um HTTP-Anfragen zu senden. Für eine Authentifizierung, setzen wir 'basic' Authentifizierung wie unten gezeigt.

```Clojure
(require '[clj-http.client :as client])

(let [creds "Benutzername:Passwort"
      encoded-creds (java.util.Base64/encoder (str creds "\n"))
      response (client/get "http://dein-webseite.de/api"
                                   {:headers {"Authorization" (str "Basic " encoded-creds)}})]
  (println (:status response))
  (println (:body response)))

```

Im Falle einer erfolgreichen Authentifizierung erhältst Du eine '200' als Status, und der Körper enthält die angeforderten Daten. 

## Tiefgang
Das Konzept der grundlegenden HTTP-Authentifizierung ist nicht neu und stammt aus der Zeit, als das Web noch in den Kinderschuhen steckte. Es bietet eine schnelle und einfache Möglichkeit zur Authentifizierung, ist aber bei weitem nicht die sicherste Methode. Alternativen sind OAuth oder das neuere Token-basierte Authentifizierungssystem JWT. Die Implementierung in Clojure ist unkompliziert, da die gesamte Authentifizierung und Codierung durch das clj-http Paket gehandhabt wird.

## Siehe Auch
Einige nützliche Links zu diesem Thema sind:

1. [Offizielle Dokumentation für clj-http](https://github.com/dakrone/clj-http)
2. [MDN Webdokumentation zu HTTP-Authentifizierung](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication)
3. [Ein Artikel über JWT-Authentifizierung](https://jwt.io/introduction/)

Beachte bitte, dass grundlegende Authentifizierung wie hier gezeigt, nur so sicher ist wie die Verbindung selbst. Bei Übertragung über das offene Internet sollte immer eine gesicherte Verbindung (HTTPS) verwendet werden.