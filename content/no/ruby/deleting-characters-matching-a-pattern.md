---
title:    "Ruby: Slette tegn som matcher et mønster"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å slette bestemte tegn eller bokstaver i en streng. Dette kan være for å gjøre teksten mer lesbar eller for å filtrere ut uønsket innhold. Ved å lære hvordan man kan slette tegn og bokstaver som matcher et gitt mønster, kan man effektivt forenkle og filtrere teksten.

## Hvordan gjøre det

Det er flere måter å slette tegn og bokstaver som matcher et mønster i Ruby på. En enkel metode er å bruke metoden `delete!`, som endrer strengen direkte istedenfor å returnere en ny streng. Dette sparer minne og forbedrer ytelsen.

```Ruby
# Fjerner alle tall fra strengen
string = "42 er et flott tall"
string.delete!("0-9")
puts string #=> " er et flott tall"
```

En annen metode er å bruke `gsub!` som står for "global substitution". Denne metoden tillater å erstatte alle forekomster av et mønster med et nytt tegn eller en tom streng.

```Ruby
# Fjerner alle vokaler fra strengen
string = "Hei, jeg liker å synge"
string.gsub!("a|e|i|o|u|y", "")
puts string #=> "H, jg lkr å sng"
```

## Dypere innsikt

I Ruby kan man bruke såkalte "regular expressions" eller regEx for kort, for å spesifisere mønstre som skal matches. Dette åpner opp for stor fleksibilitet og presisjon når det kommer til å slette bokstaver og tegn. For eksempel kan man bruke regEx for å slette bokstaver som kommer etter et bestemt tegn.

```ruby
# Fjerner alle bokstaver etter "g" i en streng
string = "Jeg elsker koding"
string.gsub!(/g.*/, "")
puts string #=> "Jeg elsker"
```

Det er også mulig å bruke regEx for å slette spesifikke sekvenser av bokstaver eller tegn.

```ruby
# Fjerner "el" hvor som helst i strengen
string = "Jeg elsker elger"
string.gsub!("el", "")
puts string #=> "Jg sker gr"
```

RegEx kan virke komplisert i begynnelsen, men med øvelse vil man raskt bli mer komfortabel med å bruke det for å slette bokstaver og tegn som matcher bestemte mønstre.

## Se også

- [Ruby's `delete!` metode](https://ruby-doc.org/core-2.7.2/String.html#method-i-delete-21)
- [Ruby's `gsub!` metode](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub-21)
- [Introduction to Regular Expressions in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)