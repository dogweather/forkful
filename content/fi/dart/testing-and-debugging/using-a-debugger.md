---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:44.735118-07:00
description: "Kuinka: **1. Katkaisukohtien asettaminen:** Asettaaksesi katkaisukohdan,\
  \ klikkaa vain koodirivin vasenta reunaa IDE:ss\xE4si (esim. Visual Studio Code\
  \ tai\u2026"
lastmod: '2024-04-05T22:38:56.875319-06:00'
model: gpt-4-0125-preview
summary: "**1. Katkaisukohtien asettaminen:** Asettaaksesi katkaisukohdan, klikkaa\
  \ vain koodirivin vasenta reunaa IDE:ss\xE4si (esim. Visual Studio Code tai Android\
  \ Studio), jossa haluat suorituksen pys\xE4htyv\xE4n."
title: "Debuggerin k\xE4ytt\xF6"
weight: 35
---

## Kuinka:


### Perusvirheenkorjaus:
**1. Katkaisukohtien asettaminen:**

Asettaaksesi katkaisukohdan, klikkaa vain koodirivin vasenta reunaa IDE:ssäsi (esim. Visual Studio Code tai Android Studio), jossa haluat suorituksen pysähtyvän.

```dart
void main() {
  var message = 'Hei, Virheenkorjaus';
  print(message); // Aseta katkaisukohta tähän
}
```

**2. Virheenkorjauksen aloittaminen:**

IDE:ssäsi, aloita virheenkorjausistunto klikkaamalla virheenkorjauskuvaketta tai painamalla virheenkorjauspainiketta. Suoritus pysähtyy katkaisukohtiin.

**3. Muuttujien tarkastelu:**

Kun suoritus on pysähtynyt, vie hiiri muuttujien päälle nähdäksesi niiden nykyiset arvot.

**4. Koodin läpikäyminen:**

Käytä IDE:ssäsi yksi askel yli, yksi askel sisään ja yksi askel ulos -komentoja navigoidaksesi koodisi läpi yhden rivin tai funktion kerrallaan.

### Edistynyt virheenkorjaus Observatorion avulla:
Dart sisältää työkalun nimeltä Observatory virheenkorjaukseen ja Dart-sovellusten profilointiin. Se on erityisen hyödyllinen sovelluksille, jotka ajetaan Dart VM:llä.

**Observatorion käyttöönottaminen:**

Aja Dart-sovelluksesi `--observe`-lipulla.

```bash
dart --observe ohjelmasi.dart
```

Tämä komento tulostaa URL-osoitteen konsoliin, jonka voit avata verkkoselaimessa päästäksesi käsiksi Observatory-virheenkorjaukseen.

### Suosittujen kolmansien osapuolien kirjastojen käyttö:
Flutter-sovellusten virheenkorjaukseen `flutter_devtools`-paketti tarjoaa joukon suorituskyky- ja virheenkorjaustyökaluja, jotka integroituvat sekä Dart VM:n että Flutterin kanssa.

**Asennus:**

Lisää ensin `devtools` `pubspec.yaml`-tiedostoosi `dev_dependencies`-kohtaan:

```yaml
dev_dependencies:
  devtools: any
```

**DevToolsin käynnistäminen:**

Suorita tämä komento terminaalissasi:

```bash
flutter pub global run devtools
```

Aloita sen jälkeen Flutter-sovelluksesi virheenkorjaustilassa. DevTools tarjoaa ominaisuuksia, kuten Flutter-inspektorin widget-puun analysointiin ja verkkoprofiilin verkkotoiminnan seurantaan.

### Esimerkki tuloste:
Kun osuma tapahtuu katkaisukohdassa, IDE saattaa näyttää muuttujien arvot ja pinon jäljet näin:

```
message: 'Hei, Virheenkorjaus'
```

Tehokkaasti hyödyntämällä virheenkorjaustyökaluja ja -tekniikoita Dartissa, kehittäjät voivat tunnistaa ja ratkaista ongelmia nopeammin, mikä johtaa sujuvampaan kehitysprosessiin ja vankempiin sovelluksiin.
