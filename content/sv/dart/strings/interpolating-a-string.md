---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:49.559846-07:00
description: "Str\xE4nginterpolation \xE4r processen att injicera variabelv\xE4rden\
  \ direkt i str\xE4ngar, ofta f\xF6r att skapa meningsfulla meddelanden utan omst\xE4\
  ndiga\u2026"
lastmod: '2024-03-13T22:44:37.596120-06:00'
model: gpt-4-0125-preview
summary: "Str\xE4nginterpolation \xE4r processen att injicera variabelv\xE4rden direkt\
  \ i str\xE4ngar, ofta f\xF6r att skapa meningsfulla meddelanden utan omst\xE4ndiga\
  \ konkateneringar."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Hur man gör:
I Dart är stränginterpolation enkel, och utnyttjar `$`-symbolen för att interpolera uttryck direkt inom strängliteraler:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Enkel variabelinterpolation
  print('Lär dig $name år $year!');
  // Utdata: Lär dig Dart år 2023!
  
  // Interpolation av uttryck
  print('Om två år kommer det att vara ${year + 2}.');
  // Utdata: Om två år kommer det att vara 2025.
}
```

I de fall där du har mer komplexa uttryck eller vill utföra operationer inom själva strängen, omslut uttrycket med `${}`. Dart har inga populära tredjepartsbibliotek specifikt för stränginterpolation eftersom det är välutrustat från början för att hantera varierade och komplexa scenarier.
