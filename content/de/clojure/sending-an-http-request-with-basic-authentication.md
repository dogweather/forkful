---
title:                "Senden einer HTTP-Anfrage mit Basic-Authentifizierung"
html_title:           "Clojure: Senden einer HTTP-Anfrage mit Basic-Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit Basic-Authentifizierung"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##Warum

Warum sollte jemand eine HTTP Anfrage mit Basic Authentication senden wollen? Nun, Basic Authentication ist ein einfaches und weit verbreitetes Schema für die Authentifizierung von Webanfragen. Es ermöglicht Benutzern den Zugriff auf geschützte Ressourcen durch die Verwendung von Benutzername und Passwort.

##Wie geht das?

Um eine HTTP Anfrage mit Basic Authentication zu senden, folgen wir diesen einfachen Schritten:

1. Importieren Sie das `clojure.java.net` Paket.
2. Definieren Sie eine Funktion mit dem Namen `send-request` mit zwei Parametern: `url` und `credentials`.
3. Erstellen Sie eine Verbindung zu der angegebenen URL mit dem `http-get` Befehl und speichern Sie das Ergebnis in einer Variablen.
4. Übergeben Sie die `credentials` in Form eines Strings im Format `Benutzername:Passwort` an die `set-request-property` Funktion.
5. Senden Sie die Anfrage mit der `open-reader` Funktion und speichern Sie die Ausgabe in einer Variablen namens `response`.
6. Lesen Sie den Inhalt der Antwort aus der `response` Variable mit der `slurp` Funktion und geben Sie diese zurück.

Ein Beispielcode sieht folgendermaßen aus:

```Clojure
(ns basic-auth
  (:require [clojure.java.net :as net]))

(defn send-request [url credentials]
  (let [conn (net/http-get url)
        _ (net/set-request-property conn "Authorization" (str "Basic " credentials))
        response (net/open-reader conn)]
    (slurp response)))
```

Die Verwendung dieser Funktion könnte wie folgt aussehen:

```Clojure
(def response (basic-auth/send-request "https://www.example.com/api" "username:password"))
```

Das Ergebnis würde den Inhalt der Antwort in der `response` Variable speichern, die dann weiterverarbeitet werden kann.

##Tief eintauchen

Beim Senden einer HTTP Anfrage mit Basic Authentication gibt es einige wichtige Dinge zu beachten:

- Die `credentials` müssen im Base64-Format kodiert werden, um von der `set-request-property` Funktion erkannt zu werden.
- Basic Authentication ist nicht die sicherste Methode der Authentifizierung, da die Credentials im Klartext übertragen werden. Aus diesem Grund wird empfohlen, auf andere Methoden umzusteigen, wie z.B. OAuth oder API-Keys.

##Siehe auch

- [Dokumentation zu `clojure.java.net`](https://clojuredocs.org/clojure.java.net)
- [Offizielle Clojure Webseite](https://clojure.org/)