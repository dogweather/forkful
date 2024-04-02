---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:52.088591-07:00
description: "Att tolka HTML i Clojure inneb\xE4r att man programmatiskt extraherar\
  \ information fr\xE5n HTML-dokument. Programmerare g\xF6r detta f\xF6r att f\xE5\
  \ \xE5tkomst till,\u2026"
lastmod: '2024-03-13T22:44:37.523266-06:00'
model: gpt-4-0125-preview
summary: "Att tolka HTML i Clojure inneb\xE4r att man programmatiskt extraherar information\
  \ fr\xE5n HTML-dokument. Programmerare g\xF6r detta f\xF6r att f\xE5 \xE5tkomst\
  \ till,\u2026"
title: Tolka HTML
weight: 43
---

## Vad & Varför?

Att tolka HTML i Clojure innebär att man programmatiskt extraherar information från HTML-dokument. Programmerare gör detta för att få åtkomst till, manipulera eller övervaka webbinnehåll dynamiskt, vilket automatiserar uppgifter eller matar data till applikationer.

## Hur man gör:

Clojure har inte inbyggd förmåga att tolka HTML, men du kan utnyttja Java-bibliotek eller Clojure-wrapper som `enlive` eller `hickory`. Så här använder du båda:

### Använda Enlive:

Enlive är ett populärt val för HTML-tolkning och webbskrapning. Först inkluderar du det i ditt projekts beroenden:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Sedan kan du tolka och navigera i HTML så här:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Detta kodsnutt hämtar en HTML-sida och väljer alla `<div>`-element med klassen `some-class`.

Utdata kan se ut som:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Här är lite innehåll."]})
```

### Använda Hickory:

Hickory tillhandahåller ett sätt att tolka HTML till ett format som är lättare att arbeta med i Clojure. Lägg till Hickory i dina projekts beroenden:

```clojure
[hickory "0.7.1"]
```

Här är ett enkelt exempel:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Tolka HTML till Hickory-format
(let [doc (hickory/parse "<html><body><div id='main'>Hej, världen!</div></body></html>")]
  ;; Välj div med id 'main'
  (select/select (select/id "main") doc))
```

Den här koden tolkar en enkel HTML-sträng och använder en CSS-väljare för att hitta en `div` med ID `main`.

Exempel på utdata:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hej, världen!"]}]
```

Både `enlive` och `hickory` erbjuder robusta lösningar för HTML-tolkning i Clojure, där `enlive` fokuserar mer på mallning och `hickory` betonar datatransformation.
