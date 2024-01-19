---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

# Behandle Strenger i Rust: Utvinne Delstrenger

## Hva & Hvorfor?
Utvinning av delstrenger handler om å plukke ut en del av en streng, et underutvalg av karakterene. Programmerere trenger å gjøre dette for effektivt å manipulere data og forbedre ytelsen av systemene sine.

## Hvordan:
I Rust er delstrengutvinningsprosessen til rette punkt, takket være innebygde metoder. Her er et enkelt eksempel.

```Rust
fn main() {
    let text = "Hei Verden";
    let delstreng = &text[0..3];
    println!("{}", delstreng);
}
```

Når du kjører dette programmet, vil det gi følgende utdata:

```Rust
"Hei"
```

Husk at Rust bruker byteindekser, ikke tegnindekser. Det kan være et problem med flerbytekarakterer (som emojis eller noen ikke-engelske tegn).

## Dyp Dykk
Strengbehandling har blitt grunnleggende siden tidlige dager av programmering, og historisk har det vært mange måter å ekstrahere delstrenger på. I Rust har vi muligheten til å bruke slice-metoden, som vist ovenfor, eller metoden "split", som kan være nyttig i noen scenarier.

Det er viktig å merke seg at Rust er utf-8 kompatibel. Utf-8 er en tegnkodek som kan representere ethvert universelt-tegnsett ISO 10646-tegn, og er den foretrukket måten å representere tekst på på Internett.

Hva gjelder ytelse, mens slicing er raskere (O(1)), kan det være lurt å være forsiktig med multibytekarakterer. Slicing kan være problematisk med tegn som opptar mer enn ett byte, fordi det kan føre til panikk hvis slicen slutter i midten av et tegn.

## Se Også
For mer informasjon om strengbehandling og utvinning av delstrenger i Rust, sjekk ut disse nyttige ressursene:
- [Den offisielle Rust dokumentasjonen](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust String eksempler på StackOverflow](https://stackoverflow.com/questions/tagged/rust+string)
- ["Using Rust For The First Time"](https://www.sitepoint.com/rust-for-first-time/) (SitePoint)
- ["Using Strings in Rust"](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misunderstandings.html#strings) (Steve Donovan's Rust Blog)