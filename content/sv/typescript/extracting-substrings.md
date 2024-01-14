---
title:    "TypeScript: Extrahering av delsträngar"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en användbar funktion i TypeScript som låter dig få ut en del av en sträng baserat på ett visst index eller ett visst antal tecken. Det är en viktig teknik att behärska för att kunna manipulera strängar i dina TypeScript-program.

## Så här gör du

För att extrahera en substräng i TypeScript använder du metoden `substring()` på en strängvariabel, och anger startindex och slutindex för den del av strängen du vill ha ut. Till exempel, om du vill extrahera de tre första tecknen i en sträng "Hej världen", använder du `substring(0, 3)` som kommer att ge dig "Hej".

```TypeScript
let sträng = "Hej världen";
let substräng = sträng.substring(0, 3);
console.log(substräng); // Hej
```

Om du bara vill ha en del av strängen baserat på ett specifikt index, kan du använda `substr()` metoden istället. Det här tillåter dig att ange startindex och antalet tecken som du vill ha ut från den strängen.

```TypeScript
let sträng = "Hej världen";
let substräng = sträng.substr(4, 6); // startar på index 4 och tar ut 6 tecken
console.log(substräng); // världen
```

Du kan också använda metoden `slice()` för att extrahera substrängar, som fungerar på samma sätt som `substring()` men låter dig också använda negativa index som räknas bakifrån. Till exempel, om du vill ha de två sista tecknen i en sträng kan du använda `slice(-2)`.

```TypeScript
let sträng = "Hej världen";
let substräng = sträng.slice(-2); // börjar från slutet och tar ut de två sista tecknen
console.log(substräng); // en
```

## Deep Dive

Förutom att ange start- och slutindex kan du också använda metoderna `substring()`, `substr()` och `slice()` på olika sätt för att få önskat resultat. Till exempel, du kan använda `substring()` tillsammans med `indexOf()` för att få ut delar av en sträng baserat på specifika tecken.

```TypeScript
let sträng = "Hej världen";
let startindex = sträng.indexOf("världen"); // hittar index för "världen"
let substräng = sträng.substring(startindex); // extraherar från det indexet till slutet av strängen
console.log(substräng); // världen
```

En annan användbar teknik är att kombinera `slice()` med `split()` för att få ut en del av en sträng baserat på ett visst tecken eller en viss text.

```TypeScript
let sträng = "1,2,3,4,5";
let delar = sträng.split(",");
let substräng = delar.slice(2, 4).join(","); // tar ut delar 3 och 4
console.log(substräng); // 3,4
```

Det finns många fler sätt att använda dessa metoder för att få ut önskad substräng, så var säker på att experimentera och hitta den bästa lösningen för ditt specifika fall.

## Se också

* [TypeScript String Dokumentation](https://www.typescriptlang.org/docs/handbook/strings.html)
* [MDN substring()](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
* [MDN substr()](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
* [MDN slice()](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/slice)