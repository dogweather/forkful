---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att radera tecken som matchar ett mönster är en aktion som fjernar alla instanser av specifika tecken från en sträng. Programmerare gör detta för att rensa data, t.ex., fjernar oväsentliga mellanslag eller oönskade tecken.

## Så här gör du:
Kodexempel och exempelutgång inom ```Rust ... ``` kodblock.

```Rust
fn main() {
    let old_string = "Hej, världen! #rustlang".to_string();
    let clean_string = old_string.replace("#rustlang", "");
    println!("{}", clean_string);
}
```
Detta script kommer att skriva ut "Hej, världen! ", med "#rustlang" teckenklustret borttaget.

## Djup dykning
Rust har designats för att erbjudna mer kontroll och minska fel jämfört med äldre språk. Om du vill radera tecken som matchar ett mönster, erbjuder Rust .replace() som en del av sin standardbibliotek.

Alternativ till .replace() inkluderar att använda regex (reguljära uttryck), men de är mer kostsamma när det gäller prestanda. 

När det kommer till genomförande, byter .replace() ut varje instans av mönstret med den angivna ersättningen. Om ersättningen är en tom sträng, raderar den bara alla instanser av det angivna mönstret.

## Se också
- [Rust Programmering av Steve Klabnik och Carol Nichols](https://doc.rust-lang.org/book/)
- [Rust String API-dokumentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust By Examples (Exempel av Rust)-officiell webbplats](https://doc.rust-lang.org/stable/rust-by-example/)
  
Håll ögat på dessa källor för mer information om hur du använder strängar i Rust och för att förbättra din kunskap om Rust programmering.