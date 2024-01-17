---
title:                "Scaricare una pagina web."
html_title:           "Java: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Cosa & Perché?
Il download di una pagina web è il processo di recuperare il contenuto di una pagina web da internet e salvarlo sul proprio dispositivo. I programmatori spesso lo fanno per accedere a dati o informazioni utili per i loro progetti.

Come fare:
```Java
URL url = new URL("https://www.esempio.it"); // creare un URL dell'indirizzo web
URLConnection connection = url.openConnection(); // aprire una connessione verso l'URL
InputStream input = connection.getInputStream(); // aprire uno stream di input per leggere il contenuto
BufferedReader reader = new BufferedReader(new InputStreamReader(input)); // creare un lettore per leggere i dati dalla connessione
String line;
while ((line = reader.readLine()) != null) { // leggere il contenuto linea per linea finché non si raggiunge la fine
    System.out.println(line); // stampare il contenuto della linea
}
reader.close(); // chiudere il lettore
```

Risultato di esecuzione:
```
<!DOCTYPE html>
<html>
<head>
<title>Esempio | Sito Web</title>
</head>
<body>
<h1>Benvenuti sul nostro sito!</h1>
<p>Qui potete trovare informazioni utili sulle nostre attività.</p>
</body>
</html>
```

Deep Dive:
Il download di una pagina web è diventato molto comune con l'avvento di internet. Prima, i programmatori dovevano utilizzare metodi più complessi per ottenere i dati di cui avevano bisogno, come ad esempio utilizzare protocolli di rete come FTP. Oggi, invece, è possibile accedere a qualsiasi pagina web e recuperare il suo contenuto utilizzando Java. Ci sono anche alternative al metodo mostrato sopra, come l'utilizzo di librerie specializzate che semplificano il processo di download di una pagina web.

Vedi anche:
- [Documentazione ufficiale di Java - URL](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Tutorial di programmazione Java - Download di una pagina web](https://www.javatpoint.com/how-to-download-url-in-java)
- [Libreria Jsoup per il parsing di pagine web in Java](https://jsoup.org/)