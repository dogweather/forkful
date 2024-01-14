---
title:                "C#: Eine http-Anfrage mit grundlegender Authentifizierung senden"
simple_title:         "Eine http-Anfrage mit grundlegender Authentifizierung senden"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

### Warum

In der heutigen Welt der vernetzten Systeme ist es oft notwendig, eine Verbindung zu einer externen Ressource herzustellen und dabei sicherzustellen, dass nur autorisierte Benutzer Zugriff darauf haben. Eine Möglichkeit, dies zu tun, ist durch den Einsatz einer HTTP-Anfrage mit grundlegender Authentifizierung. In diesem Blogbeitrag werden wir uns ansehen, wie man dies in C# programmieren kann.

### Wie

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in C# zu senden, müssen wir zunächst eine Instanz der "HttpClient" Klasse erstellen. Dann können wir die Methode "DefaultRequestHeaders" verwenden, um die notwendigen Authentifizierungsheader hinzuzufügen. Hier ist ein Beispielcode, der eine Anfrage an eine externe Ressource mit grundlegender Authentifizierung sendet:

```C#
// Erstelle eine Instanz der HttpClient Klasse
HttpClient client = new HttpClient();

// Füge den Authentifizierungsheader hinzu
string username = "Benutzername";
string password = "Passwort";
string authInfo = username + ":" + password;
authInfo = Convert.ToBase64String(Encoding.Default.GetBytes(authInfo));
client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", authInfo);

// Sende die HTTP-Anfrage
string url = "http://www.externeressource.com";
HttpResponseMessage response = await client.GetAsync(url);

// Lese die Antwort aus
string responseData = await response.Content.ReadAsStringAsync();
Console.WriteLine(responseData);
```

Dieser Code fügt den benötigten Basis-Autorisierungsheader hinzu und sendet dann die Anfrage an die externe Ressource. Die Antwort wird in der Variablen "responseData" gespeichert und kann dann weiterverarbeitet werden.

### Deep Dive

Das Hinzufügen einer grundlegenden Authentifizierung zu einer HTTP-Anfrage ermöglicht es uns, Benutzername und Passwort für die Authentifizierung in die Anfrageheader einzuschließen. Bevor die Anfrage an die externe Ressource gesendet wird, wird der Passwortwert in Base64 kodiert, um die Sicherheit zu erhöhen.

Es ist jedoch wichtig zu beachten, dass die grundlegende Authentifizierung nicht als sichere Möglichkeit der Authentifizierung gilt, da die Codierung des Passworts nicht verschlüsselt ist und daher möglicherweise abgefangen und entschlüsselt werden kann.

Es gibt auch Alternativen zur grundlegenden Authentifizierung, wie z.B. OAuth oder Token-basierte Authentifizierung, die als sicherere Optionen angesehen werden. Es ist wichtig, sich bewusst zu sein, dass die Wahl des richtigen Authentifizierungsschemas von der spezifischen Anwendung und den Sicherheitsanforderungen abhängt.

### Siehe auch

- [HTTP-Anfragen mit C# senden](https://www.w3schools.com/cs/cs_http.asp)
- [Grundlegende HTTP-Authentifizierung](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Sichere Verbindungen mit OAuth in C#](https://www.codeproject.com/Articles/875058/Create-and-Consume-Secure-RESTful-Service-in-NET-F)