---
date: 2024-01-20 17:46:31.935215-07:00
description: "How to: Syv\xE4sukellus: Rust ei oikeastaan puhu \"substringeista\"\
  \ vaan viipaleista, \"slices\". Historiallisesti viipaleet ovat Rustin yritys tarjota\u2026"
lastmod: '2024-04-05T22:51:10.495998-06:00'
model: gpt-4-1106-preview
summary: "Syv\xE4sukellus: Rust ei oikeastaan puhu \"substringeista\" vaan viipaleista,\
  \ \"slices\". Historiallisesti viipaleet ovat Rustin yritys tarjota turvallista\
  \ p\xE4\xE4sy\xE4 merkkijonoihin paniikkien ja v\xE4\xE4rien muistiviitteiden v\xE4\
  ltt\xE4miseksi. Erona esim. Pythoniin, Rust vaatii, ett\xE4 viipaleet ovat voimassaolevia\
  \ UTF-8 sekvenssej\xE4, joten suora indeksointi ei aina ole mahdollista. T\xE4m\xE4\
  n takia `.get(..)` metodi on k\xE4tev\xE4, sill\xE4 se palauttaa `Option`-tyypin,\
  \ joka kertoo, onnistuiko leikkaus. `.chars().collect()` ja muut metodit auttavat\
  \ iterointiin, jos tarvitaan enemm\xE4n kontrollia Unicode-merkkien yli."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to:
Kuinka tehdään:

```Rust
fn main() {
    let text = "Moi, Rust-ohjelmoija!";
    let start = 5;
    let end = 9;
    
    // Käytä indeksejä substringin saamiseksi
    let substring = &text[start..end];
    
    println!("Substring on: '{}'", substring); // "Substring on: 'Rust'"
    
    // Toisenlainen tapa käyttämällä `.get(..)` - metodia
    match text.get(start..end) {
        Some(sub) => println!("Toinen tapa: '{}'", sub), // "Toinen tapa: 'Rust'"
        None => println!("Virheelliset indeksit"),
    }
}
```

## Deep Dive
Syväsukellus: Rust ei oikeastaan puhu "substringeista" vaan viipaleista, "slices". Historiallisesti viipaleet ovat Rustin yritys tarjota turvallista pääsyä merkkijonoihin paniikkien ja väärien muistiviitteiden välttämiseksi. Erona esim. Pythoniin, Rust vaatii, että viipaleet ovat voimassaolevia UTF-8 sekvenssejä, joten suora indeksointi ei aina ole mahdollista. Tämän takia `.get(..)` metodi on kätevä, sillä se palauttaa `Option`-tyypin, joka kertoo, onnistuiko leikkaus. `.chars().collect()` ja muut metodit auttavat iterointiin, jos tarvitaan enemmän kontrollia Unicode-merkkien yli.

## See Also
Katso myös:

- Rust's official string slices documentation: [https://doc.rust-lang.org/std/primitive.str.html](https://doc.rust-lang.org/std/primitive.str.html)
- 'The Rust Programming Language' book substring section: [https://doc.rust-lang.org/book/ch04-03-slices.html#string-slices](https://doc.rust-lang.org/book/ch04-03-slices.html#string-slices)
- Rust by Example's take on strings: [https://doc.rust-lang.org/rust-by-example/std/str.html](https://doc.rust-lang.org/rust-by-example/std/str.html)
