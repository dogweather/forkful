---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva tester handlar om att verifiera att kod gör det den ska. Programmerare testar för att säkerställa kvalitet, undvika buggar och göra framtida underhåll enklare.

## Hur man gör:
I Ruby används ofta ramverket RSpec för att skriva tester. Här är ett enkelt exempel:

```Ruby
require 'rspec'

describe 'An example of a simple test' do
  it 'checks that 2 + 2 equals 4' do
    expect(2 + 2).to eq(4)
  end
end
```

För att köra testet, skriv följande i terminalen:

```Shell
$ rspec example_spec.rb
```

Förväntad utdata ska visa att testet passerar:

```
.

Finished in 0.00276 seconds (files took 0.15725 seconds to load)
1 example, 0 failures
```

## Djupdykning
Ruby började stödja automatiserade tester tidigt vilket skapade en stark testkultur. Utöver RSpec, finns alternativ som Minitest och Test::Unit, vilka också är populära. RSpec använder ett DSL (Domain-Specific Language) för att göra testkoden mer läsbar, medan Minitest förlitar sig på standard Ruby-syntax vilket kan kännas mer bekant för nya användare.
