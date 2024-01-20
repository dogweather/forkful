---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generierung von Zufallszahlen in TypeScript

## Was & Warum?

Die Erzeugung von Zufallszahlen besteht darin, einen unvorhersehbaren Wert von einer bestimmten Verteilung zu erhalten. Programmierer machen dies, um eine dynamische, unvorhersehbare Verhaltensweise in ihren Programmen zu erzeugen, z.B. in Spielen, Simulationen und Kodierungsexperimenten.

## Wie geht das?

Schauen wir uns ein simples Beispiel in TypeScript an, um eine Zufallszahl zu erzeugen:

```TypeScript
function getRandom(): number {
    return Math.random();
}

console.log(getRandom());
```

Da `Math.random()` ein Fließkommawert zwischen 0 und 1 ausspuckt, könnten Sie eine Ausgabe wie `0.7394483645177631` erwarten. 

Wenn Sie eine Zufallszahl zwischen einem bestimmten Bereich benötigen, können Sie die Funktion entsprechend modifizieren:

```TypeScript
function getRandomInRange(min: number, max: number): number {
    return min + Math.random() * (max - min);
}

console.log(getRandomInRange(10, 20));
```

In diesem Fall liegt der Wert zwischen 10 und 20.

## Vertiefender Einblick

Die `Math.random()`-Methode in JavaScript (und folglich in TypeScript) verwendet eine Mischung aus mehreren Algorithmen, um Zufallszahlen zu erzeugen, abhängig vom Browser oder der Laufzeitumgebung. Es sollte beachtet werden, dass diese Methode zwar für die meisten Anwendungsfälle ausreicht, sie jedoch nicht für kryptographische Zwecke geeignet ist.

Alternativ könnten Sie ein Paket wie `crypto-random` verwenden, das kryptographisch sichere Zufallszahlen erzeugt. In Node.js bietet die `crypto`-Bibliothek auch Funktionen zur Erzeugung kryptographisch sicherer Zufallszahlen.

## Siehe auch

1. [JavaScript Math Reference - W3Schools](https://www.w3schools.com/js/js_math.asp)
2. [The Mathematics of JavaScript - DZone](https://dzone.com/articles/the-mathematics-of-javascript)
3. [JavaScript’s Math Object Demystified - SitePoint](https://www.sitepoint.com/javascripts-math-object-demystified/)
4. [Crypto-random - npm](https://www.npmjs.com/package/crypto-random)

Bitte beachten Sie, dass, obwohl diese Links spezifisch für JavaScript sind, die meisten dieser Konzepte auch auf TypeScript anwendbar sind, da TypeScript eine Superset-Sprache von JavaScript ist.