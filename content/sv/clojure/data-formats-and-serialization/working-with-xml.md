---
date: 2024-01-26 04:29:18.403170-07:00
description: "XML \xE4r ett m\xE4rkspr\xE5k f\xF6r att koda dokument p\xE5 ett s\xE4\
  tt som \xE4r l\xE4sbart b\xE5de f\xF6r m\xE4nniskor och maskiner. Det \xE4r viktigt\
  \ inom webbtj\xE4nster,\u2026"
lastmod: '2024-03-13T22:44:37.549465-06:00'
model: gpt-4-0125-preview
summary: "XML \xE4r ett m\xE4rkspr\xE5k f\xF6r att koda dokument p\xE5 ett s\xE4tt\
  \ som \xE4r l\xE4sbart b\xE5de f\xF6r m\xE4nniskor och maskiner. Det \xE4r viktigt\
  \ inom webbtj\xE4nster,\u2026"
title: Att arbeta med XML
---

{{< edit_this_page >}}

## Vad & Varför?
XML är ett märkspråk för att koda dokument på ett sätt som är läsbart både för människor och maskiner. Det är viktigt inom webbtjänster, konfigurationsfiler och datautbyte eftersom det överför data i ett strukturerat, hierarkiskt format.

## Hur man gör:
Clojure erbjuder biblioteket `clojure.data.xml` för XML-tolkning och generering. Först, låt oss tolka lite XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Tolka XML-sträng
  (println parsed))
```
Utdata:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

För att generera XML från Clojure-strukturer:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Utdata:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Fördjupning
XML har funnits ett tag, med start i slutet av 90-talet som en förenklad version av SGML, avsedd för webbdata. Användningen exploderade med teknologier som SOAP och XHTML men fick konkurrens av JSON, som föredras för dess lättvikt och enkelhet.

Clojures tillvägagångssätt för XML håller det funktionellt och datacentrerat, vilket är sant för språkets ethos. `clojure.data.xml` är bara ett alternativ; du har `clojure.xml` för grundläggande behov, och för Java-interoperabilitet kan du använda dig av tungviktare som JAXB eller DOM4J.

Tänk på att prestanda- och minnesanvändningen vid hantering av mycket stora XML-dokument kan vara betungande. Strömmande tolkare som StAX kan hjälpa, men då behöver du gå över till Java-land för dem.

## Se även
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API för XML-behandling (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
