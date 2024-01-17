---
title:                "Testien kirjoittaminen"
html_title:           "Rust: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-tests.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Testien kirjoittaminen on yksinkertaisesti prosessi, jossa testataan ohjelmiston toimivuutta ja havaitaan mahdollisia virheitä. Ohjelmoijat tekevät sitä varmistaakseen, että heidän koodinsa toimii oikein ja estääkseen virheitä ja bugeja, jotka voivat aiheuttaa ongelmia käyttäjille.

# Miten:

```Rust
fn sum(x: i32, y: i32) -> i32 {
    x + y
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sum() {
        assert_eq!(sum(2, 2), 4);
    }
}
```

## Syväsukellus

### Historiallinen konteksti
Testaaminen on ollut osa ohjelmistokehitystä jo pitkään. Aikaisemmin testejä tehtiin manuaalisesti, mutta nykyään testaamiseen käytetään erilaisia ​​automaattisia työkaluja, kuten Rustin sisäänrakennettua testikehystä.

### Vaihtoehtoinen lähestymistapa
Vaikka on olemassa muita testausmenetelmiä, kuten integraatio- ja hyväksyntätestaus, yksikkötestaus on edelleen tärkeä osa ohjelmistokehitystä. Se auttaa ohjelmoijia tarkistamaan funktionaalisen logiikan pienissä yksiköissä ja vähentää mahdollisten virheiden määrää.

### Implementaation yksityiskohdat
Rustin testikehys käyttää assert-makroja tarkistamaan, että odotetut tulokset vastaavat todellisia tuloksia. Testit voidaan myös suorittaa useissa ympäristöissä, kuten debug- ja release-tilassa, mikä auttaa havaitsemaan mahdollisia ongelmia.

# Katso myös:

- Rustin testausdokumentaatio: [https://doc.rust-lang.org/book/ch11-00-testing.html](https://doc.rust-lang.org/book/ch11-00-testing.html)
- Yksikkötestauksen merkitys: [https://www.geeksforgeeks.org/unit-testing-its-importance/](https://www.geeksforgeeks.org/unit-testing-its-importance/)