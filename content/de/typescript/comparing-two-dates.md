---
title:    "TypeScript: Vergleich von zwei Daten"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Warum 
Das Vergleichen von zwei Daten ist eine grundlegende Funktion in der Webentwicklung. Es ermöglicht uns, zu überprüfen, ob ein Datum vor oder nach einem anderen liegt, was nützlich sein kann, wenn wir zum Beispiel Geburtstage oder Ablaufdaten überprüfen müssen. In diesem Blogbeitrag werden wir einen Einblick in die Verwendung von TypeScript zur Vergleich von zwei Daten erhalten.

# Wie geht man vor?
Um zwei Daten in TypeScript zu vergleichen, können wir die `Date`-Klasse und ihre entsprechenden Methoden nutzen. Schauen wir uns ein Beispiel an:
```
TypeScript
let date1 = new Date(2021, 11, 31);
let date2 = new Date(2021, 5, 15);

if(date1 > date2){
    console.log("date1 liegt nach date2");
} else {
    console.log("date2 liegt nach date1");
}
```
Die Ausgabe dieses Codes wird `date1 liegt nach date2 ` sein, da der Monat 11 nach dem Monat 5 liegt.

# Tiefergehende Informationen
Bei der Verwendung von `Date` in TypeScript gibt es einige wichtige Punkte zu beachten. Zum einen gibt es Unterschiede zwischen `new Date()` und `Date.now()`. Die erste Methode gibt die aktuelle Zeit und das Datum zurück, während die zweite Methode die Anzahl der Millisekunden seit dem 1. Januar 1970 zurückgibt.

Zusätzlich ist es wichtig zu beachten, dass die Monate in TypeScript von 0 bis 11 gezählt werden, was bedeutet, dass 0 für Januar und 11 für Dezember steht.

Für eine detailliertere Erklärung und weitere Beispiele zur Verwendung von `Date` in TypeScript, empfehlen wir die offizielle Dokumentation von TypeScript zu konsultieren.

# Siehe auch
- Offizielle TypeScript-Dokumentation zu `Date`: https://www.typescriptlang.org/docs/handbook/declaration-files/DeepDive.html#declaring-dates
- Beispielcode zum Vergleich von Daten: https://www.typescriptlang.org/docs/handbook/declaration-files/DeepDive.html#declaring-dates