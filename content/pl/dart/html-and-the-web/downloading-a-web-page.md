---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:40.155105-07:00
description: "Jak to zrobi\u0107: Dart oferuje pakiet `http`, popularn\u0105 bibliotek\u0119\
  \ stron trzecich do wykonywania \u017C\u0105da\u0144 HTTP. Oto podstawowy przyk\u0142\
  ad, jak go u\u017Cy\u0107 do pobrania\u2026"
lastmod: '2024-03-13T22:44:35.090018-06:00'
model: gpt-4-0125-preview
summary: "Dart oferuje pakiet `http`, popularn\u0105 bibliotek\u0119 stron trzecich\
  \ do wykonywania \u017C\u0105da\u0144 HTTP."
title: Pobieranie strony internetowej
weight: 42
---

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
