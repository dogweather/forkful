---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zastępowanie tekstu to operacje, które pozwalają nam znaleźć określony fragment tekstu i zastąpić go czymś innym. Wykorzystywane jest to na przykład do aktualizacji informacji w bazie danych, zmieniania nazw zmiennych w kodzie czy czyszczenia tekstu z niepotrzebnych znaków.

## Jak to zrobić:

W PHP do wyszukiwania i zastępowania tekstu używamy funkcji str_replace(). Poniżej jest przykład w jaki sposób to działa.

```PHP
$text = "Witaj, Świecie!";
$newText = str_replace("Witaj", "Do zobaczenia", $text);
echo $newText;
```

Po uruchomieniu powyższego kodu, wyjście wyglądałoby tak:

```
Do zobaczenia, Świecie!
```

## Wgłębna analiza

Wyszukiwanie i zastępowanie tekstu jest podstawową operacją, która ma swoje korzenie w starożytnych technologiach tekstowych, takich jak edytory tekstów linii poleceń. W PHP można to zrobić na kilka sposobów, na przykład poprzez używanie wyrażeń regularnych z pomocą funkcji preg_replace().

Ale dla większości przypadków str_replace() jest wystarczająco szybki i łatwy do zrozumienia. Wewnątrz, str_replace() iteruje wszystkie znaki w ciągu, sprawdzając, czy pasują do ciągu szukanego. Jeśli tak, zastępuje go ciągiem do zastąpienia.