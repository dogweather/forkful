---
title:                "Använda reguljära uttryck"
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Regular expressions är sökmönster för att hitta och hantera textsträngar. Programmerare använder det för att effektivisera textprocessning, validera data och skripta komplexa uppgifter.

## How to:
Exempel på användning av regular expressions i Ruby:

```Ruby
# Hitta första förekomsten av ett mönster
text = "Ruby är fantastiskt!"
match = text[/fantastiskt/]
puts match  # Output: "fantastiskt"

# Ersätta text med sub-metoden
ny_text = text.sub(/är/, 'är verkligen')
puts ny_text  # Output: "Ruby är verkligen fantastiskt!"

# Validera formatet på en e-postadress
email = "exempel@domain.com"
valid_email = /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i.match?(email)
puts valid_email  # Output: true

# Extrahera alla telefonnummer från en text
text_med_nummer = "Ring mig på 070-1234567 eller 08-7654321."
telefonnummer = text_med_nummer.scan(/\b\d{2,3}-\d{5,7}\b/)
puts telefonnummer.join(', ')  # Output: "070-1234567, 08-7654321"
```

## Deep Dive
Regular expressions (regex) skapades på 1950-talet och har sedan dess vuxit i popularitet inom programmering. Alternativ till regex inkluderar inbyggda strängfunktioner, som indexOf eller split i andra språk, men de är inte lika kraftfulla. Ruby använder Oniguruma-biblioteket för regex, vilket stödjer olika encodings och användarvänlig syntax.

## See Also
- Ruby-dokumentation om regular expressions: [https://ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Oniguruma GitHub-repository: [https://github.com/kkos/oniguruma](https://github.com/kkos/oniguruma)
- Regexp: [https://www.regular-expressions.info/ruby.html](https://www.regular-expressions.info/ruby.html)
