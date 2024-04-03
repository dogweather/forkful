---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:40.226667-07:00
description: "Parsowanie HTML oznacza przeszukiwanie struktury i zawarto\u015Bci pliku\
  \ HTML w celu wydobycia informacji. Programi\u015Bci robi\u0105 to, aby uzyska\u0107\
  \ dost\u0119p do danych,\u2026"
lastmod: '2024-03-13T22:44:35.582822-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie HTML oznacza przeszukiwanie struktury i zawarto\u015Bci pliku\
  \ HTML w celu wydobycia informacji."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Co i dlaczego?

Parsowanie HTML oznacza przeszukiwanie struktury i zawartości pliku HTML w celu wydobycia informacji. Programiści robią to, aby uzyskać dostęp do danych, manipulować treścią lub scrapować strony internetowe.

## Jak to zrobić:

Bash nie jest pierwszym wyborem do parsowania HTML, ale można to zrobić przy użyciu narzędzi takich jak `grep`, `awk`, `sed` lub zewnętrznych narzędzi takich jak `lynx`. Dla większej niezawodności użyjemy `xmllint` z pakietu `libxml2`.

```bash
# Zainstaluj xmllint, jeśli jest to konieczne
sudo apt-get install libxml2-utils

# Przykładowy HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Przykładowa strona</title>
</head>
<body>
  <h1>Witaj, Bash!</h1>
  <p id="myPara">Bash może mnie przeczytać.</p>
</body>
</html>
EOF

# Parsuj tytuł
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Tytuł to: $title"

# Wyciągnij akapit po ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Zawartość akapitu to: $para"
```

Wyjście:
```
Tytuł to: Przykładowa strona
Zawartość akapitu to: Bash może mnie przeczytać.
```

## Głębsze zanurzenie

W przeszłości programiści używali narzędzi opartych na regex, takich jak `grep`, do skanowania HTML, ale było to nieporęczne. HTML nie jest regularny - jest kontekstowy. Tradycyjne narzędzia nie zauważają tego i mogą być podatne na błędy.

Alternatywy? Sporo. Python z Beautiful Soup, PHP z DOMDocument, JavaScript z parserami DOM - języki z bibliotekami zaprojektowanymi, aby rozumieć strukturę HTML.

Użycie `xmllint` w skryptach bash jest solidne do prostych zadań. Rozumie XML, a przez rozszerzenie XHTML. Regularny HTML może być jednak nieprzewidywalny. Nie zawsze podąża za ścisłymi zasadami XML. `xmllint` zmusza HTML do działania w modelu XML, co dobrze sprawdza się dla dobrze sformułowanego HTML, ale może mieć problemy z bałaganem.

## Zobacz też

- [W3Schools - Parsowanie DOM HTML](https://www.w3schools.com/xml/dom_intro.asp): Demistyfikacja DOM HTML.
- [MDN Web Docs - Parsowanie i serializacja XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): Dla zasad parsowania XML, które mają zastosowanie do XHTML.
- [Dokumentacja Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Biblioteka Pythona do parsowania HTML.
- [Dokumentacja libxml2](http://xmlsoft.org/): Szczegóły na temat `xmllint` i pokrewnych narzędzi XML.
