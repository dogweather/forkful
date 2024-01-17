---
title:                "Att skicka en http-begäran med grundläggande autentisering."
html_title:           "Kotlin: Att skicka en http-begäran med grundläggande autentisering."
simple_title:         "Att skicka en http-begäran med grundläggande autentisering."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att man skickar ett meddelande till en server för att få tillgång till skyddad information eller funktionalitet. Programmerare gör det eftersom det är ett sätt att verifiera användarens identitet för att säkerställa att bara auktoriserade personer har tillgång till resurserna.

## Så här gör du:
För att skicka en HTTP-förfrågan med grundläggande autentisering i Kotlin, måste du först importera klassen för URL-förfrågan och klassen för Base64-encoderare. Sedan kan du skapa en URL och en anslutning för att utföra förfrågan med hjälp av en autentiseringssträng. När du har fått svar från servern kan du läsa och använda svaret enligt dina behov.

```
val url = URL("https://example.com/api/resource")
val connection: HttpURLConnection = url.openConnection() as HttpURLConnection
val username = "username"
val password = "password"
val authStr = "$username:$password"
val authEncoded = Base64.getEncoder().encodeToString(authStr.toByteArray())
connection.setRequestProperty("Authorization", "Basic $authEncoded")
val response = BufferedReader(InputStreamReader(connection.inputStream)).readText()
println(response)
```
Output:
```
{ "message": "Successfully authenticated!" }
```

## Djupdykning:
Historiskt sett användes HTTP-förfrågan med grundläggande autentisering som ett sätt att hantera autentisering på internet innan säkrare metoder som OAuth och API-nycklar blev vanliga. På grund av dess enkla och enkrypterade natur används det fortfarande i vissa applikationer. Alternativ till grundläggande autentisering inkluderar digest-autentisering, OAuth och API-nycklar.

När en HTTP-förfrågan med grundläggande autentisering görs, läggs en "Authorization" -header till i förfrågningsmeddelandet, som innehåller användarnamn och lösenordsinformationen som är krypterad i Base64-format. Servern å sin sida dekrypterar informationen och jämför den med sina autentiseringsuppgifter för att avgöra om åtkomst till resurserna ska ges.

## Se även:
- [Official Kotlin documentation for HttpURLConnection](https://kotlinlang.org/api/latest/jvm/stdlib/java.net.-
  u-r-l-connection/index.html)
- [Kotlin Base64 encoder and decoder class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.base64/index.html)
- [Tutorial on HTTP Basic authentication in Android with Kotlin](https://www.techotopia.com/index.php/Kotlin_-_Creating_HTTP_basic_authentication_clients_in_Android)