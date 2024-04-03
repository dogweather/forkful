---
date: 2024-01-20 18:01:04.279105-07:00
description: "HTTP-Anfragen mit Basisauthentifizierung f\xFCgen Zugangsdaten im Header\
  \ hinzu, um gesch\xFCtzte Ressourcen zu erreichen. Programmierer nutzen dies, um\
  \ sich bei\u2026"
lastmod: '2024-03-13T22:44:53.419317-06:00'
model: gpt-4-1106-preview
summary: "HTTP-Anfragen mit Basisauthentifizierung f\xFCgen Zugangsdaten im Header\
  \ hinzu, um gesch\xFCtzte Ressourcen zu erreichen."
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

## Was & Warum?
HTTP-Anfragen mit Basisauthentifizierung fügen Zugangsdaten im Header hinzu, um geschützte Ressourcen zu erreichen. Programmierer nutzen dies, um sich bei Webdiensten sicher zu authentifizieren.

## How to:
Hier ist ein einfaches Beispiel, das zeigt, wie man in Clojure eine HTTP-GET-Anfrage mit Basisauthentifizierung sendet:

```Clojure
(require '[clj-http.client :as client])

(defn fetch-protected-resource [url username password]
  (let [auth-str (str username ":" password)
        credentials (-> auth-str (.getBytes) java.util.Base64/getEncoder (.encodeToString))]
    (client/get url {:headers {"Authorization" (str "Basic " credentials)}})))

;; Beispielaufruf:
(fetch-protected-resource "https://protected.example.com" "meinBenutzer" "meinPasswort")
```

Die Funktion `fetch-protected-resource` führt eine Authentifizierung durch und gibt die Antwort zurück. Achtung: Nutze HTTPS, um deine Daten sicher zu übertragen.

## Deep Dive
Basic Authentication ist ein einfaches Authentifizierungsverfahren für HTTP, das bereits in den frühen Tagen des Webs eingeführt wurde. Es besteht ein Sicherheitsrisiko, wenn es nicht über HTTPS verwendet wird, da die Zugangsdaten im Klartext übertragen werden.

Alternativ kann man moderne Authentifizierungsmethoden wie OAuth verwenden, das sicherer und flexibler ist. Für interne oder weniger sicherheitskritische Anwendungen ist die Basisauthentifizierung jedoch nach wie vor eine praktikable Lösung.

In Clojure nutzt man oft die `clj-http` Bibliothek für HTTP-Requests, da sie einfach zu bedienen ist und gut in das Ökosystem integriert ist. Achte darauf, dass du immer die neueste Version verwendest, um von Sicherheitsupdates zu profitieren.

## Siehe Auch
- Die `clj-http` Dokumentation für weitere Details: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Eine Einführung in HTTP-Basic Authentifizierung: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- Mehr zu OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)
