---
title:    "Rust: Skriver en tekstfil"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor
Rust er et kraftig programmeringsspråk som har fått mye oppmerksomhet de siste årene. Det er kjent for å være raskt, sikkert og robust, og det blir stadig mer populært blant utviklere. Så hvorfor skulle noen ønske å skrive en tekstfil i Rust? Vel, det kan være et nyttig verktøy for å lagre og behandle data, som for eksempel innholdet på en nettside eller resultatene av et program.

# Hvordan
Nå som du vet hvorfor du ønsker å skrive en tekstfil i Rust, la oss ta en titt på hvordan du faktisk gjør det. Det første du må gjøre er å importere den nødvendige standardbiblioteket ved å legge til følgende kode øverst i filen din: ```use std::fs::File;```. Deretter oppretter du en ny fil ved å skrive ```let file = File::create("minfil.txt").unwrap();```. Dette vil opprette filen "minfil.txt" i samme mappe som koden din.

For å skrive til filen, bruker du ```file.write_all(b"Hei, verden!").unwrap();```. Her skriver vi "Hei, verden!" som et byte-streng ved å bruke ```b""``` før teksten vår. Men du kan selvfølgelig skrive hva som helst du vil her.

Når du er ferdig med å skrive til filen, må du lukke den for å frigjøre ressursene ved å skrive ```file.close().unwrap();```.

# Dypdykk
Det er mer enn bare å skrive en tekst til en fil i Rust. Du kan også lese fra en fil og endre eksisterende tekstfiler. Dette gjøres ved å bruke forskjellige metoder og funksjoner i standardbiblioteket, som ```read_from()``` og ```write_to()```. Det er også mulig å legge til formatering og struktur til tekstfilen din ved å bruke spesifikke metoder.

Det er også viktig å nevne at Rust har innebygde mekanismer for å håndtere feil i filbehandlingen. Dette sikrer at programmet ditt er trygt og pålitelig.

# Se også
- [Offisiell Rust dokumentasjon for å skrive til en fil](https://doc.rust-lang.org/std/fs/struct.File.html#method.write)
- [En praktisk guide til å skrive og lese fra tekstfiler i Rust](https://danielkeep.github.io/tlborm/book/blk-03-02.html)
- [Rust Cookbook: hvordan å skrive til og lese fra en fil](https://rust-lang-nursery.github.io/rust-cookbook/files.html)