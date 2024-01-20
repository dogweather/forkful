---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML-tulkitseminen on prosessi, jossa HTML-merkkijono muunnetaan muihin formaatteihin kuten DOM (Document Object Model) -rakenteet. Ohjelmoijat käyttävät tätä ymmärtääkseen ja käsitelläkseen verkkosivujen sisältöä tehokkaammin.

## Miten:

```C#
using HtmlAgilityPack;

var web = new HtmlWeb();
var doc = web.Load("http://ohjelmointi.fi"); // Vaihda URL oman tarpeen mukaan.

foreach (var node in doc.DocumentNode.DescendantsAndSelf())
{
    Console.WriteLine(node.Name);
}
```
Tässä esimerkissä käytämme HtmlAgilityPack-kirjastoa HTML:n tulkintaan. HTML:n tulostaa kaikki solmun nimet verkkosivulta "http://ohjelmointi.fi".

## Syvä sukellus:

HTML-tulkitseminen alkoi 1990-luvulla, kun WWW-kehittäjät tarvitsivat tavan käsitellä HTML-koodeja järjestäytyneemällä ja tehokkaammalla tavalla. Ohjelmoijat voivat käyttää muitakin menetelmiä, kuten regex-komennon, mutta usein tulkinta-libraarit kuten HtmlAgilityPack tarjoavat yksinkertaisemman ja tehokkaamman lähestymistavan. Implementaatio riippuu projektin vaatimuksista: esimerkiksi verkkosivun rakenteen muutos voi vaatia ohjelmoijaa mukauttamaan tulkintakoodia vastaavasti.

## Katso myös:

1. [HTMLAgilityPack-kirjasto](https://html-agility-pack.net/)
2. [Document Object Model (DOM)](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
3.Memory usage and performance considerations when parsing HTML with C# ([HTML-tulkinnan muisti ja suorituskyky C#-kielellä](https://stackoverflow.com/questions/3622457/memory-usage-and-performance-considerations-when-parsing-html-with-c))