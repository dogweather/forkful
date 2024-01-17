---
title:                "Wysyłanie żądania http"
html_title:           "Java: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie żądania HTTP jest podstawową umiejętnością każdego programisty, który chce komunikować się z internetem. Jest to proces, w którym aplikacja wysyła żądanie do serwera internetowego w celu pobrania lub przetworzenia danych. Programiści wykonują to w celu dostępu do zasobów internetowych, takich jak strony internetowe, pliki lub API.

## Jak to zrobić:
Aby wysłać żądanie HTTP w języku Java, musimy najpierw utworzyć obiekt klasy URL, który będzie reprezentować adres internetowy, do którego chcemy się połączyć. Następnie, używając obiektu HttpURLConnection, możemy otworzyć połączenie i wysłać żądanie przy użyciu metod takich jak GET, POST, PUT lub DELETE. Po otrzymaniu odpowiedzi od serwera, możemy przetworzyć otrzymane dane wykorzystując metody klasy InputStream lub BufferedReader.

Przykładowy kod do wysłania żądania GET wygląda tak:

```Java
String url = "http://www.example.com";
URL obj = new URL(url);
HttpURLConnection con = (HttpURLConnection) obj.openConnection();
con.setRequestMethod("GET");
BufferedReader in = new BufferedReader(
  new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();
while ((inputLine = in.readLine()) != null) {
  response.append(inputLine);
}
in.close();
System.out.println(response.toString());

```

Oczekiwany wynik powinien zawierać odpowiedź od serwera, taką jak HTML strony internetowej lub dane w formacie JSON.

## Głębszy zanurzenie:
Proces wysyłania żądań HTTP jest powszechnie stosowany od początku internetu. Wcześniej, popularne było wykorzystywanie protokołu FTP, ale został on zastąpiony przez HTTP jako standard komunikacji internetowej. Istnieją również inne sposoby na komunikację z serwerem, takie jak protokół HTTPS z szyfrowaniem SSL lub korzystanie z bibliotek zewnętrznych, np. Apache HttpClient czy OkHttp.

Wysyłanie żądań HTTP wymaga użycia różnych metod, w zależności od tego, co chcemy osiągnąć. Możemy wykonać zapytanie GET, aby uzyskać informacje lub POST, aby przesłać dane na serwer. Więcej informacji na temat dostępnych metod można znaleźć w dokumentacji protokołu HTTP.

## Zobacz także:
- [Dokumentacja protokołu HTTP](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
- [Klasa URL w języku Java](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/)
- [OkHttp](https://square.github.io/okhttp/)