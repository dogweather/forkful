---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:19.098508-07:00
description: "HTML:n j\xE4sent\xE4minen ohjelmoinnissa tarkoittaa tietojen kaivamista\
  \ HTML-dokumenteista. Ohjelmoijat tekev\xE4t t\xE4m\xE4n vuorovaikuttaakseen tai\
  \ kaivaakseen\u2026"
lastmod: '2024-03-09T21:06:20.178651-07:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4minen ohjelmoinnissa tarkoittaa tietojen kaivamista\
  \ HTML-dokumenteista. Ohjelmoijat tekev\xE4t t\xE4m\xE4n vuorovaikuttaakseen tai\
  \ kaivaakseen\u2026"
title: "HTML:n j\xE4sent\xE4minen"
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTML:n jäsentäminen ohjelmoinnissa tarkoittaa tietojen kaivamista HTML-dokumenteista. Ohjelmoijat tekevät tämän vuorovaikuttaakseen tai kaivaakseen verkkosisältöä tiedonkeruuta, testausta tai automatisointia varten, jopa silloin kun virallisia API:ja ei ole saatavilla.

## Kuinka:
Dart ei tarjoa sisäänrakennettua tukea HTML:n jäsentämiseen sen peruskirjastoissa. Voit kuitenkin käyttää kolmannen osapuolen pakettia, kuten `html`, HTML-dokumenttien jäsentämiseen ja manipulointiin.

Lisää ensin `html`-paketti `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  html: ^0.15.0
```

Tuo sitten paketti Dart-tiedostoosi:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Tässä on perusesimerkki merkkijonon jäsentämisestä, joka sisältää HTML:ää ja tietojen uuttamisesta:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hei, Dart!</h1>
      <p>Tämä on kappale esimerkki-HTML:ssä</p>
    </body>
  </html>
  """;

  // Jäsennä HTML-merkkijono
  Document document = parse(htmlDocument);

  // Tietojen poiminta
  String title = document.querySelector('h1')?.text ?? "Otsikkoa ei löytynyt";
  String paragraph = document.querySelector('p')?.text ?? "Kappaletta ei löytynyt";

  print('Otsikko: $title');
  print('Kappale: $paragraph');
}
```

Tuloste:

```
Otsikko: Hei, Dart!
Kappale: Tämä on kappale esimerkki-HTML:ssä
```

Oikeiden verkkosivujen kanssa vuorovaikuttamiseksi saatat yhdistää `html`-jäsentämisen HTTP-pyyntöihin (käyttäen `http`-pakettia web-sisällön noutamiseen). Tässä on nopea esimerkki:

Lisää ensin `http`-paketti yhdessä `html`-paketin kanssa:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Hae ja jäsentä sitten HTML-sivu verkosta:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Nouda verkkosivu
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Oletetaan, että sivulla on <h1>-tageja, jotka kiinnostavat sinua
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Otsikot: $headlines');
  } else {
    print('Pyyntö epäonnistui tilakoodilla: ${response.statusCode}.');
  }
}
```

Huom: Yllä näytettyä verkon kaavintatekniikkaa tulisi käyttää vastuullisesti ja noudattaen verkkosivuston käyttöehtoja.
