---
date: 2024-01-26 04:27:49.112675-07:00
description: "Jak to zrobi\u0107: Oto, jak parsowa\u0107 XML w Bashu. Narz\u0119dzia?\
  \ xmllint i xmlstarlet. Iteracja przez elementy XML? Zdecydowanie. Przyk\u0142ad\
  \ z przyk\u0142adowym wynikiem."
lastmod: '2024-03-13T22:44:35.609742-06:00'
model: gpt-4-0125-preview
summary: "Oto, jak parsowa\u0107 XML w Bashu."
title: Praca z XML
weight: 40
---

## Jak to zrobić:
Oto, jak parsować XML w Bashu. Narzędzia? xmllint i xmlstarlet. Iteracja przez elementy XML? Zdecydowanie. Przykład z przykładowym wynikiem:

```bash
# Zakładając, że xmlstarlet jest zainstalowany
# Zainstaluj za pomocą: apt-get install xmlstarlet

# Parsowanie zawartości XML
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# Wydobywanie nazw za pomocą xmlstarlet
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# Wynik powinien być:
# Apple
# Banana
```

## Dogłębna analiza
W latach 90. XML pojawił się jako prostsza alternatywa dla SGML, ale bardziej strukturalna niż HTML. Teraz ma towarzystwo – na przykład JSON, YAML. Ale XML nadal ma się dobrze, szczególnie w konfiguracjach i usługach internetowych opartych na SOAP.

Jeśli chodzi o narzędzia, xmllint jest wygodny do walidacji XML i zapytań xpath. xmlstarlet to szwajcarski scyzoryk do sztuczek z XML – zapytania, edycja, walidacja, transformacja. W skryptach bash, są superbohaterami do zadań związanych z XML.

Pod maską, xmllint używa libxml2 – parsera XML w C. Jest szybki, ale komunikaty o błędach? Krypticzne. A xmlstarlet? Rekurencyjne szablony i wsparcie dla EXSLT. Trudne do zrozumienia, ale potężne.

## Zobacz także
- [xmlsoft.org](http://xmlsoft.org/): Rzeczy dotyczące Libxml2 i xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Problemy i rozwiązania z prawdziwego świata.
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/): Podstawy XML.
