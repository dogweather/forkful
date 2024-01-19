---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-html.md"
---

{{< edit_this_page >}}

# "## Mitä ja Miksi?"

HTML:n jäsentäminen tarkoittaa HTML-koodin lukemista ja sen analysoimista. Ohjelmoijat tekevät tämän saadakseen tietoa tai muokatakseen web-sivuja.

# "## Kuinka tehdä:"

Jsentääksemme HTML:ää Javassa tarvitsemme Jsoup kirjaston. Esimerkiksi, jos haluat hakea otsikkotekstin (header) nettisivulta:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class Main {
    public static void main(String[] args) throws Exception{
        Document doc = Jsoup.connect("http://www.example.com").get();
        String title = doc.title();
        System.out.println("Otsikko : " + title);
    }
}
```

Koodin tuloste olisi:

```
Otsikko : Example Domain
```
# "## Syvempi sukellus"

HTML:n jäsentäminen kehittyi web-skrapingin myötä, metodina tiedon käsittelyyn HTML-sivuilta. Tämän lisäksi on olemassa myös muita keinoja HTML:n jäsentämiseen, kuten Regular Expressions (RegEx), mutta ne voivat olla monimutkaisempia ja vähemmän luotettavia.

Jsoup toimii lukemalla koko HTML-dokumentin, rakentaa siitä puumaisen rakenteen (DOM tree), josta dataa on helpompi hakea ja käsitellä.

# "## Katso myös"

[Jsoup kodin sivu](https://jsoup.org/)

[Ohjelmalli HTML:n jäsentämiseen RegEx:llä](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)