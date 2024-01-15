---
title:                "Skriva tester"
html_title:           "Ruby: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför det är viktigt att skriva tester när man programmerar, men den främsta anledningen är för att säkerställa att koden fungerar som den ska. Tester hjälper till att hitta och förebygga eventuella buggar och problem i koden innan de når produktion, vilket sparar tid och resurser på lång sikt.

## Så här gör du

För att skriva tester i Ruby behöver du först och främst installera ett testbibliotek såsom RSpec eller Minitest. Efter det kan du skapa en testsuite med olika testfall för att kontrollera olika delar av din kod. Se exempel nedanför:

```ruby
RSpec.describe Calculator do 
  describe "#add" do 
    it "adds two numbers together" do 
      expect(Calculator.add(5,3)).to eq(8) 
    end 
  end 

  describe "#subtract" do 
    it "subtracts one number from another" do 
     expect(Calculator.subtract(10,2)).to eq(8) 
    end 
  end 
end
```

I detta exempel skapar vi en testsuite för ett enkelt räknar-program med två metoder, #add och #subtract. Vi testar sedan att dessa metoder ger rätt svar för olika inputvärden.

## Djupdykning

När man skriver tester är det viktigt att tänka på att täcka så många olika scenarion som möjligt för att få en så fullständig testsuite som möjligt. Det är också viktigt att testa både positiva och negativa scenarion för att se till att koden fungerar oavsett vilken typ av input den får.

En annan viktig aspekt av att skriva tester är att hålla testerna uppdaterade när koden utvecklas och förändras. Det är inte ovanligt att testerna behöver uppdateras i takt med att koden utvecklas, och det är en viktig del av att ha en stabil och pålitlig kodbas.

## Se också

- https://rspec.info/ - officiell hemsida för RSpec
- http://www.rubydoc.info/gems/minitest - dokumentation för Minitest
- https://thoughtbot.com/blog/vim-ruby-debugging - guide för hur man debugger Ruby-kod i Vim