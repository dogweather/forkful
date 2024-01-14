---
title:    "Arduino: Wydobywanie podciągów"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego warto ekstrahować podłańcuchy w programowaniu Arduino?

Podłańcuchy, czyli fragmenty tekstu znajdujące się wewnątrz większego tekstu, mogą być bardzo przydatne w programowaniu Arduino. Mogą one zawierać cenne informacje, takie jak numery seryjne, hasła lub adresy IP, które można wykorzystać w dalszej części programu. Dzięki ekstrahowaniu podłańcuchów można precyzyjnie wybierać i wykorzystywać tylko potrzebne informacje, co znacznie ułatwia programowanie i zwiększa jego efektywność.

# Jak to zrobić w Arduino?

W celu ekstrahowania podłańcuchów w programowaniu Arduino, można skorzystać z funkcji substring (). Jest to funkcja, która pozwala na wybieranie określonych znaków z danego tekstu. Przykład kodu wyglądałby następująco:

```arduino
// przykładowy tekst
String tekst = "Hello World!";

// wybranie podłańcucha "World"
String podlancuch = tekst.substring(6, 11);

// wyświetlenie wybranego podłańcucha
Serial.println(podlancuch);

// output: World
```

W powyższym przykładzie funkcja substring () została wykorzystana do wybrania tekstu znajdującego się pomiędzy 6 a 11 znakiem, czyli słowo "World". Funkcja ta również pozwala na wybieranie podłańcucha z pewnym odstępem, np. co drugi znak.

# Pogłębione informacje o ekstrahowaniu podłańcuchów

Funkcja substring () jest bardzo przydatna, ale warto zauważyć, że operuje ona na obiektach typu String, a nie na zwykłych tablicach znaków. W związku z tym, w celu wykorzystania tej funkcji w programach Arduino, należy zadeklarować zmienne typu String.

Jedną z zalet funkcji substring () jest to, że można jej używać również do łączenia kilku tekstów w jeden. W tym celu wystarczy zastosować odpowiednie argumenty, np. substring (tekst1.length (), tekst1.length () + tekst2.length ()), co spowoduje połączenie tekstów "tekst1" i "tekst2".

# Zobacz również

- Dokumentacja funkcji substring () w języku Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- Przykładowe projekty wykorzystujące ekstrahowanie podłańcuchów w programowaniu Arduino: https://www.electronicshub.org/projects/arduino-substring-function-tutorial/