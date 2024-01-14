---
title:    "Swift: Komentoriviparametrien lukeminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi
Mikä motivoi lukemaan komentorivin argumentteja? Se on hyvä taito, joka auttaa saavuttamaan parempia ohjelmointitaitoja, ja se on myös välttämätöntä monissa projekteissa.

## Miten
Komentorivin argumenttien lukeminen Swiftillä on helppoa ja hyödyllistä. Käytämme tätä aliohjelmaa `printCommandLineArguments()`, joka tulostaa kaikki argumentit komentoriviltä:

```Swift
func printCommandLineArguments() {
    let arguments = ProcessInfo.processInfo.arguments
    print("Komentorivin argumentit: \(arguments)")
}
```

Esimerkiksi, jos suoritamme ohjelman seuraavilla argumenteilla:

```Shell
swift ohjelma.swift arg1 arg2 arg3
```

`printCommandLineArguments()` tulostaa:

```Shell
Komentorivin argumentit: [ohjelma.swift, arg1, arg2, arg3]
```

Näin pääset alkuun komentorivin argumenttien lukemisessa Swiftillä. Kokeile rohkeasti erilaisia argumentteja ja tutustu niiden lukemiseen lisää!

## Syvemmälle
Komentorivin argumenttien lukeminen on yksinkertainen tapa hyödyntää ohjelman suorituksen yhteydessä annettuja arvoja. Voit esimerkiksi käyttää komentorivin argumentteja ohjelman asetusten määrittämiseen tai käyttäjän syötteenä. Voit myös siirtää komentorivin argumentteja toisten aliohjelmien käyttöön.

Komentorivin argumenttien lukeminen tapahtuu `ProcessInfo` luokan avulla, joka tarjoaa pääsyn ohjelman suorituksen aikaisiin tietoihin, kuten komentorivin argumentteihin.

## Katso myös
- [Swiftin virallinen dokumentaatio](https://developer.apple.com/documentation/foundation/processinfo)
- [Veera Nymanin blogipostaus (suomeksi)](https://veeran.fi/komentorivin-parametrit-ja-kayttajalta-saatava-syote-swiftissa/)
- [Swiftin komentorivisovelluksen esimerkki GitHubissa](https://github.com/apple/swift/blob/master/utils/build-script)

Kiitos lukemisesta ja onnea komentorivin argumenttien lukemisen kanssa!