---
title:                "Java: Lähettää http-pyyntö perusautentikointia käyttäen."
simple_title:         "Lähettää http-pyyntö perusautentikointia käyttäen."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat lähettää HTTP-pyynnön perusautentikoinnilla? On tärkeää ymmärtää, että perusautentikointi tarjoaa tietoturvaa verkkosovelluksille lähettämällä käyttäjän tunnistetiedot salattuna. Tämä on erityisen tärkeää, kun käsitellään arkaluontoista tietoa.

## Kuinka tehdä

Koodiesimerkki osoittaa, kuinka lähettää HTTP-pyyntö käyttäen perusautentikointia. 

```Java
// Luodaan HttpClient -olio
HttpClient client = new DefaultHttpClient();

// Määritellään verkkopalvelimen osoite, jolle lähetämme pyynnön
String url = "http://www.example.com/api";

// Määritellään käyttäjätunnus ja salasana
String username = "käyttäjätunnus";
String password = "salasana";

// Luodaan perusautentikointi -olio käyttäjätunnuksella ja salasanalla
BasicCredentialsProvider credentialsProvider = new BasicCredentialsProvider();
credentialsProvider.setCredentials(new AuthScope(AuthScope.ANY_HOST, AuthScope.ANY_PORT), new UsernamePasswordCredentials(username, password));

// Rakennetaan pyyntö
HttpGet request = new HttpGet(url);

// Asetetaan oikea otsake (header) perusautentikoinnille
request.addHeader(new BasicScheme().authenticate(new UsernamePasswordCredentials(username, password), request, null));

// Suoritetaan pyyntö ja tallennetaan vastaus muuttujaan
HttpResponse response = client.execute(request);

// Tulostetaan vastauskoodi
System.out.println(response.getStatusLine().getStatusCode());

// Tulostetaan vastauksen sisältö
InputStream inputStream = response.getEntity().getContent();
BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
String line;
while ((line = reader.readLine()) != null) {
    System.out.println(line);
}
```

Esimerkkitulostus:

```
200
{
    "message": "Pyyntö hyväksytty",
    "content": "Tervetuloa!"
}
```

Huomaa, että käyttäjän tunnistetiedot ovat vain esimerkkikäyttöä varten ja niitä ei tulisi koskaan sisällyttää koodiin tai jakaa muiden kanssa.

## Syväsukellus

HTTP-pyynnön lähettäminen perusautentikoinnilla vaatii useita osia, kuten oikean HttpClient-olion luomisen, pyynnön rakentamisen ja vastauksen käsittelyn. On tärkeää ymmärtää, mitä jokainen osa tekee ja miksi se on tarpeellinen. Lisäksi on hyvä huomioida mahdolliset virhelähteet ja varmistaa, että käyttäjän tunnistetiedot pysyvät turvassa.

## Katso myös

- [HTTP-pyynnön lähettäminen Java-kielellä käyttäen HttpClient-rajapintaa](https://www.tutorialspoint.com/http/http_java_client.htm)
- [Perusautentikointi](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)