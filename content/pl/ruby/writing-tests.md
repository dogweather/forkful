---
title:                "Pisanie testów"
date:                  2024-01-19
simple_title:         "Pisanie testów"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Testy pozwalają sprawdzić, czy kod robi to co powinien. Dzięki nim możemy szybko wykryć błędy i zabezpieczyć aplikację przed przyszłymi problemami.

## Jak to zrobić?

```Ruby
# Zainstaluj gem 'rspec'
# W terminalu: gem install rspec

# test_spec.rb
require_relative 'twoj_kod'

RSpec.describe "An example of a test" do
  it "checks if method returns correct value" do
    expect(metoda_testowa(3)).to eq(9)
  end
end

# twoj_kod.rb
def metoda_testowa(x)
  x * x
end

# Uruchom test w terminalu:
# rspec test_spec.rb

# Oczekiwany wynik:
# .

# Finished in 0.00276 seconds (files took 0.15743 seconds to load)
# 1 example, 0 failures
```

## Głębsze spojrzenie

Testy w Ruby zaczęły być popularne po wydaniu narzędzia RSpec około 2005 roku. Alternatywy to Minitest czy Test::Unit. RSpec wykorzystuje składnię typu DSL (Domain Specific Language), co sprawia, że testy są bardziej czytelne. Implementacja testów wymaga znajomości asercji i metod 'expect', które pozwalają określić oczekiwane zachowania testowanej funkcji.

## Zobacz również

- RSpec documentation: https://rspec.info/documentation/
- RubyGuides testing tutorial: https://www.rubyguides.com/2018/07/rspec-tutorial/
- Better Specs {RSpec best practices}: http://www.betterspecs.org/

Pamiętaj, że regularna praktyka i eksploracja możliwości jest najlepszą metodą nauki pisania testów.
