---
date: 2024-01-20 17:58:41.697270-07:00
description: "S\xF6ka och ers\xE4tta text \xE4r tekniken att hitta specifika textstr\xE4\
  ngar och byta ut dem med n\xE5got annat. Programmerare g\xF6r detta f\xF6r att snabbt\
  \ uppdatera kod,\u2026"
lastmod: '2024-02-25T18:49:36.727992-07:00'
model: gpt-4-1106-preview
summary: "S\xF6ka och ers\xE4tta text \xE4r tekniken att hitta specifika textstr\xE4\
  ngar och byta ut dem med n\xE5got annat. Programmerare g\xF6r detta f\xF6r att snabbt\
  \ uppdatera kod,\u2026"
title: "S\xF6kning och ers\xE4ttning av text"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Söka och ersätta text är tekniken att hitta specifika textsträngar och byta ut dem med något annat. Programmerare gör detta för att snabbt uppdatera kod, korrigera fel eller manipulera data.

## How to: (Hur gör man:)
```Ruby
# Exempel: Söka och byta ut med String#gsub

original_text = "Jag gillar katter. Katter är fantastiska!"
replaced_text = original_text.gsub("katter", "hundar")

puts replaced_text
# Output: Jag gillar hundar. Hundar är fantastiska!
```

```Ruby
# Använda regular expressions (regex) för mönstersökning

email_text = "Kontakta oss på kontakt@example.com"
updated_text = email_text.gsub(/\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6}\b/, '[censurerad]')

puts updated_text
# Output: Kontakta oss på [censurerad]
```

## Deep Dive (Djupdykning)
Sök och ersätt har sina rötter i tidiga textbehandlingsprogram. I Ruby görs ofta detta med `String#gsub`, som kan ta enkel sträng eller ett reguljärt uttryck, vilket ger flexibilitet. Alternativen inkluderar string metoden `sub` som ersätter endast första träffen, samt verktyg som `sed` i Unix. När vi använder `gsub!` görs ändringarna direkt på originalsträngen, vilket påverkar minnesanvändningen.

## See Also (Se också)
- Ruby Documentation on `String#gsub` and `String#sub`: [Ruby Docs](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- Regular Expressions in Ruby: [Ruby Regexp](https://ruby-doc.org/core-2.7.1/Regexp.html)
- The `sed` stream editor in Unix for file manipulations: [sed manual](https://www.gnu.org/software/sed/manual/sed.html)
