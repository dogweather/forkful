---
title:                "Praca z XML"
aliases:
- pl/fish-shell/working-with-xml.md
date:                  2024-01-26T04:30:57.863462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML oznacza manipulowanie danymi w powszechnym, strukturyzowanym formacie, używanym w konfiguracjach, komunikacji i wielu innych. Programiści manipulują XML-em, aby odczytywać, zapisywać, aktualizować i wyszukiwać dane — co jest kluczowe dla interoperacyjności w wielu aplikacjach i usługach.

## Jak to zrobić:
Fish nie ma wbudowanego parsowania XML, więc będziesz polegać na zewnętrznych narzędziach, takich jak `xmllint` lub `xmlstarlet`. Oto fragment kodu do odczytu wartości:

```fish
# Parsowanie XML za pomocą xmlstarlet
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Wynik:
```
Hello World
```

Aby edytować XML, użyj tego:

```fish
# Edycja elementu XML za pomocą xmlstarlet
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

Wynik:
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## Wnikliwe spojrzenie:
XML istnieje od końca lat '90, stworzony z myślą o czytelności i przyjazności dla maszyn. Chociaż JSON odebrał część popularności XML-a z powodu swojej prostoty, XML pozostaje zakorzeniony tam, gdzie ważne są walidacja dokumentów i przestrzenie nazw.

Alternatywy? Oczywiście — JSON, YAML czy nawet binarne formaty, takie jak Protocol Buffers dla tych wysokowydajnych aplikacji. Ale XML schema i XSLT (do transformacji XML) mogą być decydujące w skomplikowanych scenariuszach, gdzie liczy się solidność.

Pod maską, narzędzia takie jak `xmlstarlet` korzystają z potężnych bibliotek, takich jak libxml2, dając ci XPath i XQuery do precyzyjnego majsterkowania w XML. Nie są to tylko narzędzia XML, ale bramy do manipulacji DOM, ponieważ zastosujesz podobne koncepty w dowolnym języku, który styka się z XML-em.

## Zobacz również:
- [Dokumentacja xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Dokumentacja Fish](https://fishshell.com/docs/current/index.html)
- [Funkcje i operatory XPath i XQuery](https://www.w3.org/TR/xpath-functions/)
