---
title:    "Ruby: Skrivning till standardfel"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är ett viktigt verktyg för alla Ruby-programmerare. Genom att skriva ut information till standard error istället för standard output kan du enkelt separera ut felsökningsinformation från vanlig användardata. Detta gör det möjligt att hitta och åtgärda fel med din kod snabbt och effektivt.

## Hur man gör

Det finns flera sätt att skriva till standard error i Ruby. Här är två enkla exempel:

```Ruby
# Metod 1
$stderr.puts "Detta skrivs till standard error"
$stdout.puts "Detta skrivs till standard output"

# Metod 2
puts "Detta skrivs till standard output"
STDERR.puts "Detta skrivs till standard error"
```

När du kör detta kodexempel kommer den första raden ("Detta skrivs till standard error") att skrivas ut i terminalen som röd text medan den andra raden ("Detta skrivs till standard output") kommer att skrivas ut som vanlig text. Detta gör det enkelt att särskilja mellan felsökningsinformation och användarinformation.

## Djupdykning

Varför använda $stderr.puts istället för puts? Anledningen är att puts använder standard output (STDIO) kanal, medan $stderr.puts använder standard error (STDERR) kanal. Det finns tillfällen när du vill skriva ut information som är avsedd för användaren (t.ex. resultatet av en beräkning) och information som är avsedd för utvecklare (t.ex. felmeddelanden). Genom att skriva ut felsökningsinformation till standard error kan du enkelt filtrera ut dessa med hjälp av redirect syntaxen (>). Till exempel:

```Ruby
ruby script.rb > output.txt # Skriver ut allt till en fil
ruby script.rb 2> errors.txt # Skriver ut endast fel till en fil
```

Detta kan vara särskilt användbart när du kör ett program på en server där du kanske inte har tillgång till utskrifter på skärmen.

## Se även

- [Redirecting standard error in Ruby](https://stackoverflow.com/questions/5888573/redirecting-standard-error-in-ruby)
- [The Ruby Standard Library](https://ruby-doc.org/stdlib-2.7.1/) (sektionen om standard error)
- [Använda standard error i dina Ruby-program](https://blog.bigbinary.com/2012/04/18/using-stderr-to-log-output-and-errors-in-ruby.html)