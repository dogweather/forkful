---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) on mahdollisuus raportoida virheitä ja varoituksia. Käytämme sitä, koska haluamme erottaa normaalin ulostulon (stdout) virheistä, mikä auttaa debuggauksessa ja loggauksessa.

## How to:
Stderr:iin kirjoittaminen Rustissa:

```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "Tapahtui virhe!").unwrap();
}
```

Tuloste näyttäisi esimerkiksi tältä:

```
Tapahtui virhe!
```

Jos haluat käyttää `eprintln!`-makroa, koodi on yksinkertaisempi:

```Rust
fn main() {
    eprintln!("Tapahtui toinen virhe!");
}
```

Tuloste on sama kuin ensimmäisessä esimerkissä.

## Deep Dive
Stderr juontaa juurensa Unix-järjestelmiin, joissa oli erilliset kanavat normaalille ulostulolle ja virheilmoituksille. Rustissa voit käyttää `std::io`-kirjastoa stderr:iin kirjoittamiseen tai `eprintln!`-makroa, joka on lyhyempi tapa tulostaa virheilmoituksia. `writeln!`-makro taas antaa enemmän kontrollia, kuten mahdollisuuden käsitellä virheitä Write-traitin `write_fmt`-metodin kautta.

## See Also
Rust-dokumentaatio stderr:stä:
- [std::io::stderr](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [std::fmt](https://doc.rust-lang.org/std/fmt/)

Unix standard streams -historia:
- [Wikipedia: Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
