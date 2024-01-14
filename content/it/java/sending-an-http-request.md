---
title:                "Java: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché inviare una richiesta HTTP è importante

In un mondo in cui la comunicazione online è essenziale, la capacità di inviare richieste HTTP è fondamentale per lo sviluppo di applicazioni web. Grazie alle richieste HTTP, è possibile comunicare con i server e accedere alle risorse necessarie per il corretto funzionamento dell'applicazione.

## Come inviare una richiesta HTTP in Java

Per inviare una richiesta HTTP in Java, è necessario utilizzare la classe `HttpURLConnection` e il metodo `openConnection()`. Di seguito è riportato un esempio di codice che mostra come effettuare una richiesta GET e visualizzare il codice di stato e il corpo della risposta:

```Java
URL url = new URL("https://www.example.com");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET");

int responseCode = con.getResponseCode();

System.out.println("Codice di stato: " + responseCode);

BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuilder responseBody = new StringBuilder();
while ((inputLine = in.readLine()) != null) {
    responseBody.append(inputLine);
}
in.close();

System.out.println("Corpo della risposta: " + responseBody.toString());
```

L'output del codice precedente dovrebbe essere simile a questo:

```
Codice di stato: 200
Corpo della risposta: <html>
<head>
<title>Example Domain</title>
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.</p>
</body>
</html>
```

## Approfondimento sull'invio di richieste HTTP

Le richieste HTTP possono contenere diversi metodi, tra cui GET, POST, PUT e DELETE, che consentono di specificare l'azione da eseguire sul server. Inoltre, possono includere intestazioni (headers) e body, che possono trasportare informazioni aggiuntive e dati da inviare al server. È importante comprendere in modo approfondito questi aspetti per utilizzare al meglio le richieste HTTP nelle applicazioni web.

## Vedi anche

- [Documentazione ufficiale Java sulla classe HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Tutorial su come utilizzare le richieste HTTP in Java](https://www.baeldung.com/java-http-request)
- [Esempio di implementazione di richieste GET e POST in Java](https://www.geeksforgeeks.org/implementation-get-and-post-methods-http-using-java/)