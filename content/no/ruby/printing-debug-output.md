---
title:    "Ruby: Utskrift av feilsøkingsutdata"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å printe ut debug-utgang er en viktig del av å skrive kode i Ruby, spesielt når du jobber med større og mer komplekse prosjekter. Det kan hjelpe deg med å finne og fikse feil og gjøre koden din mer effektiv og lesbar.

## Slik gjør du det

For å printe ut debug-utgang i Ruby, kan du bruke metoden `p`. Denne metoden tar et argument og printer det ut med et linjeskift. Du kan også bruke `puts` eller `print` metoder, men `p` er spesifikt designet for debug-utgang og vil vise mer informasjon om objektet du printer ut.

```Ruby
a = 5
p a # => 5
```

Du kan også printe ut flere verdier eller variabler ved å separere dem med kommaer:

```Ruby
a = 5
b = "hello"
p a, b # => 5, "hello"
```

En annen måte å få mer detaljert debug-utgang på, er å bruke `inspect` metoden. Denne metoden vil gi deg en detaljert beskrivelse av objektet du printer ut, inkludert dets klasse og verdi.

```Ruby
a = [1,2,3]
p a.inspect # => "[1, 2, 3]"
```

## Dykk dypere

For mer avansert debugging kan du også bruke `binding.pry` eller `binding.irb` inne i koden din. Disse vil stoppe kjøringen av programmet og åpne et interaktivt konsollmiljø på det tidspunktet. Her kan du utforske og inspisere variabler og objekter for å finne feil eller se hvordan de blir endret.

```Ruby
def add_two_numbers(a, b)
  result = a + b
  binding.pry
  return result
end

add_two_numbers(3, 4) # => [1] pry(main)> result
                       # => 7
```

## Se også

- [Ruby metoder: p, puts, print](https://www.rubyguides.com/2018/10/p-puts-print/)
- [Debugging Ruby applikasjoner med pry](https://rossta.net/blog/debugging-ruby-applications-with-pry.html)
- [Åtte måter å få mer ut av Ruby's `p` metode](https://thoughtbot.com/blog/using-the-pp-method-for-more-helpful-debugging-output)