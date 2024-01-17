---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Clojure: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Was ist das und warum machen wir das?

Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung ist, wenn wir eine Anfrage an einen Server senden, die unsere Benutzername und Passwort enthält, um unseren Zugriff auf die Ressource zu überprüfen. Programmierer machen das, um sicherzustellen, dass nur authentifizierte Benutzer auf ihre Inhalte zugreifen können.

Wie geht das?

```Clojure
;; Importiere die required-namespace Bibliothek
(require '[clojure.java.io :as io])
;; Erstelle eine neue HTTP-Anfrage mit Authentifizierung
(def req (io/request 
		"http://www.example.com/posts" 
		:method :get 
		:basic-auth {:username "john" :password "mypassword"}))
;; Sende die Anfrage und erhalte die Antwort
(io/finish (io/post req))

```

Hier sehen wir, wie wir eine HTTP-Anfrage mit grundlegender Authentifizierung senden und eine Antwort von der angegebenen URL erhalten. Wir verwenden die "clojure.java.io" Bibliothek, um die Anfrage zu erstellen und zu senden. Die "basic-auth" Option ermöglicht es uns, unsere Benutzername und Passwort hinzuzufügen.

Tiefes Eintauchen

Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung ist nicht immer die sicherste Methode, da Benutzername und Passwort im Klartext übertragen werden. Eine Alternative ist die Verwendung von OAuth2, das eine sicherere Authentifizierung ermöglicht, ohne dass Benutzernamen und Passwort übertragen werden müssen.

In Clojure ist es auch möglich, die Sitzung und Cookies zu verwalten, um die Authentifizierung zwischen verschiedenen Anfragen aufrechtzuerhalten. Dies kann mit der Bibliothek "clj-http" erreicht werden, die auch eine einfache Möglichkeit bietet, HTTP-Anfragen mit grundlegender Authentifizierung zu senden. Darüber hinaus gibt es die Möglichkeit, benutzerdefinierte Header für zusätzliche Authentifizierungsinformationen hinzuzufügen.

Siehe auch

- "Leitfaden für OAuth2 in Clojure" - Ein detaillierter Leitfaden zur Verwendung von OAuth2 in Clojure.
- "clj-http" - Eine Bibliothek zur Verwaltung von HTTP-Anfragen in Clojure.
- "Clojure Cookbook" - Kurze Beispiele und Snippets für häufige Aufgaben in Clojure, einschließlich des Sendens von HTTP-Anfragen mit Authentifizierung.