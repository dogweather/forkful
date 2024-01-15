---
title:                "Zufallszahlen generieren"
html_title:           "C#: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Es mag seltsam erscheinen, aber das Generieren von Zufallszahlen ist in der Programmierung sehr nützlich. Von der Erstellung von zufälligen Passwörtern bis hin zur Simulation von Daten, die Verwendung von Zufallszahlen ist eine häufige Anforderung in verschiedenen Anwendungsbereichen.

## Wie geht's

Um zufällige Zahlen in C# zu generieren, gibt es zwei Hauptansätze: die Verwendung der `Random`-Klasse oder die Verwendung von kryptographischen Funktionen. Schauen wir uns beide an:

### Verwendung der `Random`-Klasse

Die `Random`-Klasse ist eine integrierte Klasse in C#, die uns bei der Generierung von Pseudo-Zufallszahlen hilft. Sie kann auf zwei Arten verwendet werden:

1. Mit einem vorgegebenen Startwert:

```C#
// Initialisierung mit vorgegebenem Startwert
Random rnd = new Random(42);

// Generierung einer zufälligen Ganzzahl zwischen 0 und 100
int randomNumber = rnd.Next(0, 101);

// Generierung einer zufälligen Dezimalzahl zwischen 0 und 1
double randomDouble = rnd.NextDouble();
```

2. Ohne vorgegebenen Startwert:

```C#
Random rnd = new Random();

// Generierung einer zufälligen Ganzzahl zwischen 0 und 100
int randomNumber = rnd.Next(0, 101);

// Generierung einer zufälligen Dezimalzahl zwischen 0 und 1
double randomDouble = rnd.NextDouble();
```

Es ist wichtig zu beachten, dass die Generierung von Zufallszahlen auf Basis von `Random` eigentlich nicht wirklich zufällig ist, sondern auf einem Algorithmus basiert, der einen Startwert verwendet. Wenn du also immer die gleiche Sequenz von Zahlen haben möchtest, kannst du die `Random`-Klasse mit einem spezifischen Startwert initialisieren.

### Verwendung von kryptographischen Funktionen

Wenn du hochsichere Zufallszahlen benötigst, z.B. für Passwörter oder kryptographische Schlüssel, ist es besser, kryptographische Funktionen zu verwenden. In der Klasse `RNGCryptoServiceProvider` gibt es eine Methode namens `GetBytes()`, die eine Byte-Array mit zufälligen Werten zurückgibt:

```C#
// Initialisierung der kryptographischen Klasse
RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider();

// Erstellen eines Byte-Arrays mit 16 zufälligen Werten für ein sicheres Passwort
byte[] randomNumber = new byte[16];
rng.GetBytes(randomNumber);
```

Der Nachteil dieser Methode ist, dass sie wesentlich langsamer ist als die Verwendung der `Random`-Klasse.

## Tiefergehende Infos

Wie bereits erwähnt, ist die Zufallszahlengenerierung auf Basis von `Random` eigentlich nicht wirklich zufällig. Sie basiert auf mathematischen Algorithmen, die einen Startwert verwenden und eine deterministische Sequenz von Zahlen generieren. In den meisten Fällen ist dies ausreichend, aber wenn es um Sicherheit geht, ist es besser, kryptographische Funktionen zu verwenden.

Eine gute Möglichkeit, zufällige Zahlen auf Basis von `Random` zu überprüfen, ist die Verwendung einer Statistikklasse, um die Verteilung der generierten Zahlen zu analysieren. Wenn die Verteilung gleichmäßig ist, bedeutet dies, dass der verwendete Algorithmus gute Ergebnisse liefert.

## Siehe auch

- [Offizielle Dokumentation zur Random-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.random?view=netcore-3.1)
- [Artikel über das Generieren sicherer Passwörter in C#](https://blog.elmah.io/how-to-generate-random-password-in-c-sharp/)