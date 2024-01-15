---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Kotlin: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché inviare una richiesta HTTP con autenticazione di base?

Ci sono molte situazioni in cui è necessario accedere ad un servizio web attraverso un'API che richiede la verifica delle credenziali dell'utente. In questi casi, l'autenticazione di base è un metodo comune e semplice per garantire l'accesso ai propri dati.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Kotlin, è necessario prima di tutto importare la libreria Apache HttpClient nel tuo progetto. Puoi farlo aggiungendo questa dipendenza al tuo file di configurazione gradle:

```Kotlin
dependencies{
   implementation "org.apache.httpcomponents:httpclient:4.5.9"
}
```

Una volta importata la libreria, puoi utilizzarla per creare la tua richiesta HTTP. Ad esempio, per inviare una richiesta GET a un servizio web protetto da autenticazione di base, puoi usare il seguente codice:

```Kotlin
val username = "username"
val password = "password"
val url = "https://example.com/api/data"
val httpClient = HttpClientBuilder.create().build()

val credentials = UsernamePasswordCredentials(username, password)
val authScope = AuthScope(url, AuthScope.ANY_PORT)
val credentialProvider = BasicCredentialsProvider().also {
   it.setCredentials(authScope, credentials)
}

val httpGet = HttpGet(url)
httpClient.addDefaultCredentialsProvider(credentialProvider)

val httpResponse = httpClient.execute(httpGet)
val responseContent = String(httpResponse.entity.content.readBytes())

println(responseContent)
```

L'output di questo esempio sarà il contenuto della risposta del servizio web.

## Deep Dive

Per inviare una richiesta HTTP con autenticazione di base, il client deve inviare un'intestazione "Authorization" nella sua richiesta, contenente il valore "Basic" seguito dalle credenziali utente codificate in base64. Inoltre, è necessario conoscere l'URL del servizio web e le credenziali dell'utente per autenticarsi correttamente.

Una nota importante è che l'autenticazione di base non è considerata sicura ed è sconsigliata quando si tratta di dati sensibili. In questi casi, è consigliato utilizzare metodi di autenticazione più robusti come OAuth o JWT.

## See Also

- [Apache HttpClient Documentazione](https://hc.apache.org/httpcomponents-client-5.1.x/index.html)
- [Autenticazione di Base su Wikipedia](https://it.wikipedia.org/wiki/Basic_access_authentication)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/)