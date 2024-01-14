---
title:    "Rust: Virheenjäljitystulosteen tulostaminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi Tulostaa Debug-tulosteita?

Rust on moderni, tehokas ja turvallinen ohjelmointikieli, joka mahdollistaa monipuolisen ohjelmoinnin ja suorituskyvyn optimoinnin. Debug-tulosteiden tulostaminen on kätevä tapa tutkia ja seurata koodin suoritusta vaihe vaiheelta sekä tunnistaa mahdollisia virheitä. Se on myös hyödyllinen työkalu ohjelmiston kehittäjille, jotka haluavat ymmärtää paremmin ohjelmansa toimintaa.

## Kuinka Tulostaa Debug-tulosteita?

Debug-tulosteiden tulostaminen Rustissa on helppoa ja intuitiivista. Voit käyttää `println!` -makroa tai `eprintln!` -makroa tulostamaan viestin standardilähtöön tai virhelokiin. Voit myös käyttää `dbg!` -makroa tulostamaan muuttujan arvon ja sen arvon tyypin tarkemman tutkimuksen yhteydessä.

```Rust
let name = "Carl";
println!("Hello, {}", name); // tulostaa "Hello, Carl" terminaaliin
dbg!(name); // tulostaa "name = "Carl"" ja sen tyypin virhelokiin
```

Voit myös muotoilla tulosteita `format!` -makron avulla luomalla ensin viestin merkkijonon ja antamalla sitten muuttujien arvot halutuissa paikoissa.

```Rust
let number = 42;
let message = format!("The answer is {}!", number);
println!("{}", message); // tulostaa "The answer is 42!"
```

## Syvemmälle Tulostamisen Saloihin

Rustin debug-tulostusten avulla voit myös tutkia monimutkaisempia tietorakenteita, kuten vektoreita ja hajautustauluja. Voit käyttää `{:?}` -muotoilumerkintää tulostamaan tietorakenteen sisällön kaikilla tasoilla. Voit myös käyttää `{:?}` -merkintää yhdessä `dbg!` -makron kanssa saadaksesi tarkempia tietoja tietorakenteen sisällöstä.

```Rust
let numbers = vec![1, 2, 3];
dbg!(numbers); // tulostaa "numbers = [1, 2, 3]" virhelokiin
dbg!(format!("{:?}", numbers)); // tulostaa "numbers = [1, 2, 3]" virhelokiin
```

Voit myös muotoilla `println!`- ja `format!`-makrojen avulla merkkijonoja ennen niiden tulostamista, kuten käyttämällä uudelleenrivin merkintää `\n` ja tabulointia `\t`.

```Rust
let name = "Carl";
println!("Hello, {}\nWelcome to Rust!", name); // tulostaa "Hello, Carl" ja "Welcome to Rust!" kahdelle riville
let number = 12345;
println!("The number is {}\n\tand it has {} digits.", number, number.to_string().len()); // tulostaa "The number is 12345" ja "\tand it has 5 digits." kahdelle riville
```

## Katso Myös

- [Rustin virallinen debug-tulosteiden dokumentaatio](https://doc.rust-lang.org/std/fmt/)
- [Rust By Example -opas debug-tulosteiden tulostamiseen](https://doc.rust-lang.org/rust-by-example/hello/print.html)
- [Debug-tulosteiden käyttö Michael Snoymanin blogissa](https://www.snoyman.com/blog/2018/04/introduction-debugging-rust)