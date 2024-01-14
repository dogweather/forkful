---
title:                "Java: Tekstin etsiminen ja korvaaminen"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Monissa Java-ohjelmoinnin projekteissa, on tarpeen etsiä ja korvata tekstiä tietyistä tiedostoista tai tietokannoista. Tämä helpottaa tekstin muokkaamista ja parantaa ohjelman suorituskykyä.

## Miten tehdä

Voit käyttää Java-koodia etsimään ja korvaamaan tekstiä helposti. Seuraavassa esimerkissä näet, kuinka yksinkertaisen tiedoston sisältävän tekstin voi etsiä ja korvata. 

```Java
String content = "Tervetuloa! Tämä on Java-blogi.";
String newText = content.replace("Tervetuloa", "Hei");

System.out.println(newText);

// Output: Hei! Tämä on Java-blogi.
```

Voit myös etsiä ja korvata tekstiä tiedostosta käyttämällä Scanner-luokkaa ja File-luokkaa. Seuraava esimerkki näyttää, kuinka voit etsiä tiettyä sanaa ja korvata sen toisella tekstillä. 

```Java
Scanner scanner = new Scanner(new File("tiedosto.txt"));
while (scanner.hasNextLine()) {
    String line = scanner.nextLine();
    if (line.contains("Java")) {
        line = line.replace("Java", "JavaScript");
    }
    System.out.println(line);
}
```

Jos haluat löytää ja korvata tekstiä tietokannasta, voit käyttää SQL-kyselyitä. Tässä esimerkissä korvaamme kaikki "John" nimiset henkilöt taulukossa "asiakkaat" nimellä "Juhani". 

```Java
String query = "UPDATE asiakkaat SET nimi = 'Juhani' WHERE nimi = 'John'";
Statement statement = connection.createStatement();
int rowsUpdated = statement.executeUpdate(query);
System.out.println("Updated " + rowsUpdated + " rows.");
```

## Syvällinen tarkastelu

Java tarjoaa monia erilaisia tapoja etsiä ja korvata tekstiä, sekä erilaisia käyttötarkoituksia kuten tiedostot, tietokannat tai käyttäjän syöttämä teksti. On tärkeää ymmärtää eri vaihtoehdot ja valita sopivin tapa sen mukaan minkä tyyppistä tekstiä haluat etsiä ja korvata. Samalla on myös hyvä ottaa huomioon ohjelman suorituskyky sekä turvallisuus.

## Katso myös

- [Java String.replace()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-java.lang.CharSequence-java.lang.CharSequence-)
- [Java Scanner-luokka](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Java File-luokka](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java SQL-kyselyt](https://docs.oracle.com/javase/tutorial/jdbc/basics/processingsqlstatements.html)