---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-Anfrage mit einfacher Authentifizierung in C#: Ein kurzer Überblick

## Was & Warum?

(HTTP "Anzahl der Protokolle") ist ein Mittel zur Übertragung von Daten über das Internet, wobei eine einfache Authentifizierung als Sicherheitsmaßnahme dient. Dies wird von Programmierern verwendet, um sicherzustellen, dass sensibler Inhalt nur von autorisierten Benutzern abgerufen wird.

## So Geht's:

Wenn Sie eine HTTP-Anfrage mit einfacher Authentifizierung in C# senden möchten, können Sie den folgenden Code als Referenz verwenden.

```C#
using System;
using System.Net;
using System.Text;

class Program {
  static void Main() {
    var url = "http://example.com";
    var username = "username";
    var password = "password";
    
    var encoded = Convert.ToBase64String(Encoding.GetEncoding("ISO-8859-1").GetBytes(username + ":" + password));

    var request = WebRequest.Create(url);
    request.Headers.Add("Authorization", "Basic " + encoded);
    
    using (var response = request.GetResponse()) {
      Console.WriteLine("Status: " + ((HttpWebResponse)response).StatusCode);
    }
  }
}
```

Die Ausgabe wird den Status der Anfrage anzeigen. Zum Beispiel:

```
Status: OK
```

## Tiefer Tauchen:

Obwohl die einfache Authentifizierung aus Sicht der Implementierung sehr geradlinig ist, sollten Sie sie nur unter bestimmten Bedingungen verwenden. Historisch gesehen war sie die erste Methode zur Implementierung der Authentifizierung in HTTP, aber sie bietet heutzutage wesentlich weniger Sicherheitsmaßnahmen als modernere Verfahren, wie beispielsweise Tokenbasierte Authentifizierung oder OAuth.

Es gibt mehrere Alternativen zur einfachen Authentifizierung, darunter Digest-Authentifizierung, Forms-Based-Authentifizierung und Integrated Windows-Authentifizierung. Jede Methode hat ihre Vor- und Nachteile, und die Wahl hängt von den spezifischen Anforderungen Ihres Projekts ab.

Im C#-Beispielcode verwenden wir "ISO-8859-1" als Encoding, dies ist auch als "Latin1" bekannt und ist eine Art Zeichen-Codierung, die für viele westeuropäische Sprachen geeignet ist. Sie könnten jedoch auch ein anderes Encoding verwenden, abhängig von den Authentifizierungsdetails Ihrer Anwendung.

## Siehe Auch:

Für zusätzliche Informationen können die folgenden Quellen nützlich sein:

1. [HTTP-Autorisierung](https://tools.ietf.org/html/rfc7235)
2. [Verschiedene Authentifizierungsschemata](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication)
3. [HTTP-Clients auf MSDN](https://docs.microsoft.com/de-de/dotnet/api/system.net.http.httpclient)