---
title:                "Rust: Vergleich zweier Daten"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Die Vergleichung von zwei Daten mag auf den ersten Blick vielleicht nicht besonders aufregend klingen, aber es ist ein grundlegender Aspekt der Datumsmanipulation in der Programmierung. Ob Sie nun eine Kalenderfunktion für eine Anwendung erstellen oder Daten filtern möchten, die Verwendung von Vergleichsoperatoren für Daten ist unerlässlich. In diesem Blog-Beitrag werden wir uns ansehen, wie man in Rust zwei Daten vergleichen kann und warum dies von Bedeutung ist.

## Wie Geht Man Vor

Um zwei Daten in Rust zu vergleichen, werden wir die eingebauten Vergleichsoperatoren verwenden, wie zum Beispiel `==` (gleich), `!=` (ungleich), `<` (kleiner als), `>` (größer als), `<=` (kleiner oder gleich) und `>=` (größer oder gleich). Nehmen wir als Beispiel zwei Variablen mit Datumsangaben:

```
let date_1 = "2021-01-20";
let date_2 = "2021-01-25";
```

Um diese beiden Daten zu vergleichen, können wir die oben genannten Vergleichsoperatoren verwenden:

```
println!("Ist date_1 gleich date_2? {}", date_1 == date_2); // false
println!("Ist date_1 ungleich date_2? {}", date_1 != date_2); // true
println!("Ist date_1 größer als date_2? {}", date_1 > date_2); // false
println!("Ist date_1 kleiner oder gleich date_2? {}", date_1 <= date_2); // true
```

Wie Sie sehen, können wir mit diesen einfachen Vergleichsoperatoren schnell und einfach zwei Daten vergleichen. Es ist wichtig zu beachten, dass die verwendeten Datumsformate übereinstimmen sollten, da es sonst zu unerwarteten Ergebnissen kommen kann.

## Tiefer Eintauchen

Obwohl die oben genannten Vergleichsoperatoren gut geeignet sind, gibt es bestimmte Situationen, in denen eine tiefere Auseinandersetzung mit der Vergleichung von Daten erforderlich sein kann. Zum Beispiel kann es vorkommen, dass wir das Datum in ein bestimmtes Format konvertieren müssen, bevor wir es vergleichen können. Dafür können wir die in Rust eingebaute `DateTime`-Bibliothek verwenden. Hier ist ein Beispiel, wie wir ein Datum von einem String in ein `DateTime`-Objekt konvertieren können:

```
use std::str::FromStr;
use chrono::{DateTime, Utc, TimeZone};

let date_1 = DateTime::from_str("2021-01-20 00:00:00").unwrap();
let date_2 = DateTime::from_str("2021-01-25 00:00:00").unwrap();
```

Nun können wir diese beiden Daten mit den oben genannten Vergleichsoperatoren vergleichen, da sie beide im gleichen Format vorliegen. Allerdings müssen wir nun auch die Zeitzone berücksichtigen, da `DateTime` standardmäßig in UTC arbeitet. Hier ist ein Beispiel für den Vergleich unter Berücksichtigung der Zeitzone:

```
let date_2_with_timezone = Utc.from_local_datetime(&date_2.naive_local()).unwrap();

println!("Ist date_1 größer als date_2? {}", date_1 > date_2_with_timezone); // false
```

Nun erhalten wir das korrekte Ergebnis, da wir die Zeitzone berücksichtigt haben.

# Siehe Auch

- [Rust Dokumentation über Vergleichsoperatoren](https://doc.rust-lang.org/std/cmp/trait.PartialOrd.html)
- [Chrono Dokumentation zur Datumsmanipulation in Rust](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Playground zum Ausprobieren](https://play.rust-lang.org/)