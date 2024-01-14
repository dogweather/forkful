---
title:                "Kotlin: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler er en viktig del av moderne programmering. Dette gjøre det mulig for applikasjoner å kommunisere med servere og utveksle data. Uansett om du utvikler en nettside, en mobilapplikasjon eller et backend-system, vil du mest sannsynlig måtte sende HTTP-forespørsler på et tidspunkt.

## Hvordan

For å sende en HTTP-forespørsel i Kotlin, kan du bruke biblioteket "OkHttp". Først må du legge til avhengigheten i "build.gradle"-filen din:

```Kotlin
dependencies { 
    implementation("com.squareup.okhttp3:okhttp:4.9.0") 
}
```

Deretter kan du opprette en "OkHttpClient" og en "Request" for å sende forespørselen:

```Kotlin
val client = OkHttpClient()
val request = Request.Builder().url("https://example.com").build()
```

Du kan også legge til eventuelle parametere eller innhold i forespørselen ved hjelp av "Builder"-klassen. Når du er ferdig, kan du kalle "newCall" på "OkHttpClient" og få responsen som en "Response"-objekt:

```Kotlin
val response = client.newCall(request).execute()
println(response.body?.string()) // Skriver ut innholdet i responsen
```

Output:

```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title> 
  ...
</head>
<body>
  <h1>Example Domain</h1> 
  <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
  ...
  ...
</body> 
</html>
```

## Dypdykk

For å sende en HTTP-forespørsel må du også ta hensyn til ulike HTTP-metoder, som "GET", "POST", "PUT", "DELETE", osv. Du bør også være oppmerksom på eventuelle sikkerhets- eller autentiseringsprosedyrer som kreves for å få tilgang til serveren.

OkHttp tilbyr også mange avanserte funksjoner, som asynkrone forespørsler, caching, og responshåndtering. Du kan utforske disse videre i dokumentasjonen til biblioteket.

## Se også

- [Dokumentasjon for OkHttp](https://square.github.io/okhttp/)
- [Kotlin offisielle nettside](https://kotlinlang.org/)
- [Tutorials om Kotlin programmering](https://www.programiz.com/kotlin)