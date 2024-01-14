---
title:                "Arduino: Zapisywanie wiersza z wielkiej litery"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu, mamy do czynienia z różnymi typami danych, takimi jak napisy (stringi). Czasami, w celu czytelniejszego wyświetlenia danych lub ze względów estetycznych, chcemy aby wybrane napisy były w formie zaczynającej się z dużej litery. W takim przypadku, przydatnym narzędziem jest funkcja "capitalize", która pozwala na zmianę pierwszej litery napisu na dużą.

## Jak to zrobić

Aby skorzystać z funkcji "capitalize" w Arduino, musimy najpierw zadeklarować zmienną typu "String" i przypisać do niej wybrany ciąg znaków. Następnie używamy funkcji "capitalize" do zmiany pierwszej litery napisu na dużą. Przykład zastosowania wygląda następująco:

```Arduino 
String napis = "arduino";
napis.capitalize();
```

Po wykonaniu powyższych instrukcji, zmienna "napis" będzie miała wartość "Arduino".

## Głębszy zanurzenie

Oprócz funkcji "capitalize", istnieje również inna metoda na zmianę pierwszej litery napisu na dużą, która wymagało by wykorzystania funkcji "charAt" do dostępu do poszczególnych liter w napisie. Jednak ta metoda jest bardziej skomplikowana i wymaga większej ilości kodu.

Warto również zaznaczyć, że funkcja "capitalize" jest dostępna tylko dla zmiennej typu "String", a nie dla typów prostych, takich jak "int" czy "float".

## Zobacz również

- Dokumentacja funkcji "capitalize": https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/capitalize/
- Inne metody na manipulację napisami w Arduino: https://create.arduino.cc/projecthub/projects/tags/string
- Dyskusja na forum Arduino na temat funkcji "capitalize":https://forum.arduino.cc/index.php?topic=548875.0