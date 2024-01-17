---
title:                "Html:n jäsentäminen"
html_title:           "Javascript: Html:n jäsentäminen"
simple_title:         "Html:n jäsentäminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Vedonlyöjät saattavat ihmetellä, mitä tarkalleen ottaen HTML-analysointi tarkoittaa. Se on yksinkertaisesti tapa lukea ja käsitellä HTML-koodia verkkosivuilta. Ohjelmoijat tekevät tätä usein automatisoidun työn helpottamiseksi, kuten tiettyjen elementtien löytämiseksi ja datan keräämiseksi verkkosivuilta.

## Miten:
Seuraavassa on esimerkki koodista, jonka avulla voit lukea ja tulostaa HTML-koodia käyttämällä JavaScriptia:

```Javascript
const parser = new DOMParser();
const html = "<html><body><h1>Tervetuloa!</h1></body></html>";
const doc = parser.parseFromString(html, "text/html");
const title = doc.querySelector("h1").innerHTML;
console.log(title); //tulostaa "Tervetuloa!"
```

## Syväsukellus:
HTML-analysointi on ollut olennainen osa verkkosivujen luomista ja analysointia alusta alkaen. Ennen JavaScriptin keksimistä JavaScriptin avulla voitiin analysoida vain staattista dataa. Nyt on olemassa myös muita vaihtoehtoja, kuten erilaisia kirjastoja ja ohjelmistoja, jotka voivat auttaa HTML-koodin analysoinnissa. HTML-analysointiin kuuluu myös useita erilaisia tekniikoita ja algoritmeja, joita voidaan käyttää analysoinnin helpottamiseksi.

## Katso myös:
- [Creating an HTML Parser using JavaScript](https://www.codeproject.com/Articles/395453/Creating-an-HTML-Parser-using-JavaScript)
- [A Beginner's Guide to Parsing HTML using JavaScript](https://blog.bitsrc.io/a-beginners-guide-to-parsing-html-using-javascript-9c6d43dc4c25)
- [Introduction to HTML parsing using JavaScript](https://www.internalpointers.com/post/introduction-html-parsing-using-javascript)