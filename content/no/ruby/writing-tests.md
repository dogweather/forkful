---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester betyr å lage kode som sjekker om annen kode fungerer som forventet. Programmerere gjør det for å sikre kvalitet og stabilitet i sine applikasjoner og forenkle vedlikehold og oppdateringer.

## Hvordan gjøre det:
I Ruby bruker vi ofte RSpec for testing. Her er et enkelt eksempel:

```Ruby
require 'rspec'

describe 'Simple Math' do
  it 'adds two numbers' do
    expect(2 + 3).to eq(5)
  end
end
```

Når du kjører denne testen, bør du se følgende output:

```
.

Finished in 0.00276 seconds (files took 0.15724 seconds to load)
1 example, 0 failures
```

## Fordypning
Testing i Ruby har utviklet seg over tid, med rammeverk som Test::Unit som forfedre. Alternativer til RSpec inkluderer Minitest og Cucumber. Viktige implementeringsdetaljer i testing inkluderer mocking/stubbing og testdrevet utvikling (TDD).

## Se også
- RSpec dokumentasjon: [https://rspec.info/documentation/](https://rspec.info/documentation/)
- Minitest: [https://github.com/seattlerb/minitest](https://github.com/seattlerb/minitest)
- Cucumber: [https://cucumber.io/docs](https://cucumber.io/docs)