---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:21.485494-07:00
description: "Att skriva till standardfel (stderr) i Ruby handlar om att dirigera\
  \ felmeddelanden eller diagnostik till en separat utmatningsstr\xF6m, skild fr\xE5\
  n\u2026"
lastmod: '2024-02-25T18:49:36.761866-07:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i Ruby handlar om att dirigera felmeddelanden\
  \ eller diagnostik till en separat utmatningsstr\xF6m, skild fr\xE5n\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) i Ruby handlar om att dirigera felmeddelanden eller diagnostik till en separat utmatningsström, skild från standardutmatningen (stdout). Programmerare gör detta för att skilja regelbunden programutmatning från fel och felsökningsinformation, vilket underlättar diagnos av problem och parsning av loggar.

## Hur man gör:
Rubys standardbibliotek ger ett enkelt sätt att skriva till stderr med `$stderr` eller `STDERR`. Du behöver inte tredjepartsbibliotek för denna grundläggande operation.

### Skriva ett enkelt meddelande till stderr:
```ruby
$stderr.puts "Fel: Filen hittades inte."
# Eller motsvarande
STDERR.puts "Fel: Filen hittades inte."
```
Exempel på utmatning (till stderr):
```
Fel: Filen hittades inte.
```

### Omdirigera stderr till en fil:
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Misslyckades med att öppna konfigurationen."
end
```
Denna kodsnutt omdirigerar stderr till en fil med namnet `error.log`, och alla efterföljande skrivna fel kommer att skrivas ut där tills programmet återställer stderr-omdirigeringen eller avslutas.

### Använda stderr med undantagshantering:
```ruby
begin
  # Simulerar en operation som kan misslyckas, t.ex. att öppna en fil
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "Undantag inträffade: #{e.message}"
end
```
Exempel på utmatning (till stderr):
```
Undantag inträffade: Ingen sådan fil eller katalog @ rb_sysopen - nonexistent_file.txt
```

Även om Rubys inbyggda metoder för att skriva till stderr räcker för många applikationer, för mer komplexa loggningsbehov, kanske du överväger standardbiblioteket `logger` eller externa gems som `Log4r`. Dessa ger konfigurerbara loggningsmekanismer, inklusive allvarlighetsnivåer, formatering och förmågan att skriva till olika utmatningar, inklusive filer, e-post och mer.
