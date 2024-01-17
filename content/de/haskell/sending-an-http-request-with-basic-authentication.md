---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Haskell: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum? 
Das Versenden einer HTTP-Anfrage mit grundlegender Authentifizierung ist eine Methode, um sicherzustellen, dass der Server den Client kennt und die Benutzeridentität überprüfen kann. Programmierer verwenden dies, um auf geschützte Ressourcen zuzugreifen oder um sicherzustellen, dass nur autorisierte Benutzer Daten anfordern können.

## Wie geht's?
Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Haskell zu senden, folgen Sie diesen Schritten:

1. Importieren Sie das ``` Network.HTTP ``` Modul.
2. Erstellen Sie eine ``` Request ``` Datenstruktur mit der angeforderten URL und der verwendeten Methode (z.B. GET oder POST).
3. Verwenden Sie die ``` setRequestMethod ``` Funktion, um die verwendete Authentifizierungsmethode festzulegen (z.B. Basic).
4. Erstellen Sie eine ``` Authorization ``` Datenstruktur mit den Benutzerdaten (z.B. Benutzername und Passwort).
5. Verwenden Sie die ``` buildRequest ``` Funktion, um die Anfrage mit der Authentifizierung zu erstellen.
6. Verwenden Sie die ``` simpleHTTP ``` Funktion, um die Anfrage an den Server zu senden.
7. Verarbeiten Sie die Antwort des Servers mit der ``` getResponseBody ``` Funktion.

Ein vollständiges Beispiel könnte folgendermaßen aussehen:

```Haskell
import Network.HTTP

url = "https://www.example.com"
username = "beispielbenutzer"
password = "geheimnis123"

-- Schritt 2
request = Request {rqURI = url, rqMethod = GET}

-- Schritt 3
requestWithMethod = setRequestMethod "Basic" request

-- Schritt 4
authorization = Authorization (Just (username, password))

-- Schritt 5
authenticatedRequest = buildRequest requestWithMethod authorization

-- Schritt 6
getResponse = simpleHTTP authenticatedRequest >>= getResponseBody

-- Schritt 7
main = do
  response <- getResponse
  print response
```

Die Ausgabe könnte ähnlich aussehen:

```Haskell
"<html><head><title>Willkommen bei Beispiel</title></head><body>Hallo, Beispielbenutzer!</body></html>"
```

## Tiefere Einblicke
Die grundlegende Authentifizierung wurde bereits 1999 in RFC 2617 standardisiert und ist eine der ältesten Methoden für die Sicherung von HTTP-Anfragen. Sie ist einfach zu implementieren, da nur Benutzername und Passwort in Base64-codierter Form im HTTP-Header übertragen werden. Allerdings ist diese Methode unsicher, da die Informationen leicht von einem Angreifer abgefangen werden können.

Es gibt verschiedene Alternativen zur grundlegenden Authentifizierung, wie z.B. die Digest-Authentifizierung oder OAuth. Diese Methoden bieten zusätzliche Sicherheit, indem sie z.B. verschlüsselte Passwörter oder Tokens verwenden.

Die Implementation einer HTTP-Anfrage mit grundlegender Authentifizierung in Haskell ist relativ einfach, da das ``` Network.HTTP ``` Modul bereits alle nötigen Funktionen bereitstellt. Es ist jedoch wichtig, sicherzustellen, dass die Übertragung der Benutzerdaten verschlüsselt erfolgt, um die Sicherheit zu erhöhen.

## Siehe auch
Weitere Informationen über das Versenden von HTTP-Anfragen mit grundlegender Authentifizierung in Haskell finden Sie in der offiziellen Dokumentation des ``` Network.HTTP ``` Moduls und in der RFC 2617 Spezifikation.