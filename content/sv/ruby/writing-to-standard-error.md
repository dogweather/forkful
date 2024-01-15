---
title:                "Skriva till standardutskrift"
html_title:           "Ruby: Skriva till standardutskrift"
simple_title:         "Skriva till standardutskrift"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Varför
Att skriva till standard error kan vara användbart för att lägga till felsökningsmeddelanden i ditt Ruby-program. Det kan också vara en bra idé att skriva till standard error istället för standard output för att hålla ditt program mer organiserat och lättläst.

##Hur man gör det
Det är enkelt att skriva till standard error med Ruby. Allt du behöver göra är att använda `STDERR.puts` följt av det meddelande du vill skriva ut. Till exempel:

```Ruby
STDERR.puts "Detta är ett felmeddelande!"
```

Detta kommer att skriva ut "Detta är ett felmeddelande!" till standard error. Om du vill skriva ut flera meddelanden kan du använda `STDERR.print`, som fungerar på samma sätt som `puts` men utan att lägga till en ny rad efter varje meddelande.

##Djupdykning
När du skriver till standard error i Ruby, skrivs meddelandena ut i rött istället för i svart som standard output. Detta gör det enklare att särskilja felsökningsmeddelanden från vanliga utskrifter. Det är också viktigt att notera att när du skriver till standard error, kommer meddelandena skrivas ut oavsett om ditt program körs från terminalen eller från en fil.

En annan användbar funktion är `STDERR.inspect`, vilket gör det möjligt att skriva ut en detaljerad representation av ett objekt eller en variabel till standard error. Till exempel:

```Ruby
array = [1, 2, 3]
STDERR.puts STDERR.inspect(array)
```

Detta kommer att skriva ut `[1, 2, 3]` till standard error och du kan enkelt kontrollera värdet på dina variabler.

##Se även
Läs mer om standard error i Ruby genom att utforska dessa länkar:

- https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-STDERR
- https://ruby-doc.org/core-2.7.1/IO.html#method-c-new-label-IO+to+Other
- https://stackify.com/how-to-redirect-stdout-to-file-in-ruby/