---
date: 2024-01-20 17:58:41.697270-07:00
description: "How to: (Hur g\xF6r man:) ."
lastmod: '2024-03-13T22:44:38.413749-06:00'
model: gpt-4-1106-preview
summary: .
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

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
