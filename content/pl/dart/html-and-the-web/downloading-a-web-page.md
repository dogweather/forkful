---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:40.155105-07:00
description: "Pobieranie strony internetowej polega na pobraniu zawarto\u015Bci strony\
  \ internetowej za pomoc\u0105 jej adresu URL w celu przetworzenia lub przechowania.\u2026"
lastmod: '2024-03-13T22:44:35.090018-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie strony internetowej polega na pobraniu zawarto\u015Bci strony\
  \ internetowej za pomoc\u0105 jej adresu URL w celu przetworzenia lub przechowania.\u2026"
title: Pobieranie strony internetowej
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej polega na pobraniu zawartości strony internetowej za pomocą jej adresu URL w celu przetworzenia lub przechowania. Programiści robią to, aby ekstrahować informacje, monitorować zmiany lub archiwizować zawartość, co czyni to podstawą w web scraping, data mining i zautomatyzowanych testach.

## Jak to zrobić:

Dart oferuje pakiet `http`, popularną bibliotekę stron trzecich do wykonywania żądań HTTP. Oto podstawowy przykład, jak go użyć do pobrania strony internetowej:

Najpierw dodaj pakiet `http` do twojego `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Następnie zaimportuj pakiet i użyj go do pobrania zawartości strony internetowej:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var odpowiedz = await http.get(url);
  if (odpowiedz.statusCode == 200) {
    print('Strona pobrana:');
    print(odpowiedz.body);
  } else {
    print('Żądanie nie powiodło się ze statusem: ${odpowiedz.statusCode}.');
  }
}
```

**Przykładowe wyjście** (to będzie się różnić w zależności od zawartości strony internetowej):

```
Strona pobrana:
<!doctype html>
<html>
<head>
    <title>Przykładowa Domena</title>
...
</html>
```

W bardziej złożonych scenariuszach, takich jak obsługa ciasteczek czy ustawianie nagłówków agencji użytkownika, używałbyś tego samego pakietu `http`, ale z dodatkowymi konfiguracjami dla twojego żądania:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var naglowki = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'nazwa=wartość; nazwa2=wartość2',
  };
  var url = Uri.parse('http://example.com');
  var odpowiedz = await http.get(url, headers: naglowki);

  if (odpowiedz.statusCode == 200) {
    print('Strona pobrana z niestandardowymi nagłówkami:');
    print(odpowiedz.body);
  } else {
    print('Żądanie nie powiodło się ze statusem: ${odpowiedz.statusCode}.');
  }
}
```

Używanie nagłówków w ten sposób może dokładniej imitować żądania przeglądarki, co jest szczególnie użyteczne przy dealingu ze stronami, które mają specyficzne wymagania lub ochrony przed scrapingiem.
