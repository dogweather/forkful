---
title:    "Rust: Skriva till standardfel"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standarderror, eller stderr, är ett viktigt verktyg för alla som programmerar i Rust. Det är ett sätt att få information om eventuella fel eller varningar som kan uppstå i koden och hjälper till att felsöka problem på ett strukturerat sätt.

## Hur man gör det

För att skriva till stderr i Rust, måste du först importera standardbiblioteket io::Write. Sedan kan du använda write! makron för att skriva önskat meddelande till stderr. Exempelvis:

```Rust
use std::io::Write;

fn main() {
    let message = "Detta är ett meddelande till stderr";
    write!(std::io::stderr(), "{}", message).unwrap();
}
```

Output: `Detta är ett meddelande till stderr`

Här använder vi unwrap() för att avsluta makronet. Det finns också andra sätt att hantera potentiella fel, som att använda en match-sats eller en ? operator. Det är viktigt att hantera fel på ett korrekt sätt för att undvika kraschar och oväntat beteende i koden.

## Djupdykning

När du använder write! makronet för att skriva till stderr, kan du också ange färg och formatering för meddelandet. Detta kan vara användbart för att skilja mellan olika typer av meddelanden och lättare kunna läsa dem. Exempelvis kan du skriva:

```Rust
use std::io::Write;

fn main() {
    let message = "Detta är ett meddelande till stderr";
    write!(std::io::stderr().paint("ERROR:"), "{}", message).unwrap();
}
```

Output:  ERROR: Detta är ett meddelande till stderr

Du kan också använda andra formatmallar som färg, bakgrundsfärg, fetstil och understrykning för att anpassa dina meddelanden.

## Se också

- Rust's official documentation för standardbiblioteket för att läsa mer om io::Write
- Rust By Example för fler exempel på hur man skriver till stderr med olika formatteringar
- Dokumentation för fylogenetik biblioteket för ett praktiskt exempel på användningen av stderr i ett större projekt