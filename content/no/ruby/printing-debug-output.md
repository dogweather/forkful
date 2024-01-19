---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utskrift av feilretting, eller 'debug output', er en programmeringsteknikk hvor kode vises i sanntid mens programmet kjører. Det er viktig for utviklere fordi det hjelper med å spore og rette feil i koden raskt.

## Hvordan:
Her er hvordan du kan skrive ut debug-utdata i Ruby:

```ruby
print "debug: x har nå verdien: #{x}\n"
```

Når du kjører denne koden, vil den skrive ut den nåværende verdien av x.

La oss se på et eksempel med en løkke:

```ruby
for x in 1..5
  print "debug: x har nå verdien: #{x}\n"
end
```
Eksempel utdata vil være:

```
debug: x har nå verdien: 1
debug: x har nå verdien: 2
debug: x har nå verdien: 3
debug: x har nå verdien: 4
debug: x har nå verdien: 5
```

## Dyp Dykk

Historisk sett, har utskrift av debugging vært en viktig del av programmering siden dagene med hulkort og unix-terminaler. I Ruby, `puts` og `print` funksjonene er vanligvis brukt til dette formålet, men det finnes mer avanserte biblioteker som `debugger` og `byebug`.

Alternativt kan du også bruke Logger biblioteket:

```ruby
require 'logger'

logger = Logger.new(STDOUT)
logger.debug("x har nå verdien: #{x}")
```

Hovedforskjellen mellom å skrive ut direkte og å bruke Logger er at Logger gir deg større kontroll over hvor og hvordan utskriften vises. Du kan også rotere logger-filer for å spare på diskplass.

## Se Også

Hvis du vil lære mer om debugging og utskrift av debug-utdata i Ruby, sjekk ut disse nyttige lenkene:

1. [Ruby Debugging Guide](https://www.rubyguides.com/2015/06/ruby-debugging/)
2. [The Logger Library](https://ruby-doc.org/stdlib-2.5.1/libdoc/logger/rdoc/Logger.html)
3. [Byebug Debugger](https://github.com/deivid-rodriguez/byebug)