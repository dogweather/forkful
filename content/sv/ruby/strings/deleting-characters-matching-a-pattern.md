---
title:                "Ta bort tecken som matchar ett mönster"
aliases: - /sv/ruby/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:08.134217-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ta bort tecken som matchar ett mönster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
I Ruby kan du rensa strängar från oönskade tecken genom att matcha dem mot ett mönster. Programmerare gör detta för att städa data, validera input eller bearbeta text på ett specifikt sätt.

## How to:
I Ruby använder du `String#gsub` och `String#delete` för att ta bort tecken som matchar ett mönster. Här är några exempel:

```Ruby
# Använda gsub för att ta bort alla siffror från en sträng
str = "Bilmärke 2020"
puts str.gsub(/\d/, '')  # Output: "Bilmärke "

# Använda delete för att ta bort specifika tecken
str = "hello#%world!"
puts str.delete("#%")    # Output: "helloworld!"
```

`gsub` kan använda reguljära uttryck, så det är kraftfullt och flexibelt. Medan `delete` är snabb och enkel när du vet exakt vilka tecken du vill ta bort.

## Deep Dive:
Att bearbeta strängar är grundläggande i programmering. I Ruby, som skapades 1995 av Yukihiro Matsumoto, fick vi `String#gsub` och `String#delete` som en del av dess rika API för att hantera text.

`String#gsub` står för "global substitution" och ersätter alla förekomster som matchar ett mönster. Eftersom det kan ta reguljära uttryck, kan det vara både en fördel och en nackdel — kraftfull men potentiellt långsammare och svårare att läsa.

`String#delete` är direktare och snabbare men mindre flexibelt. Det tar helt enkelt en lista av tecken som ska tas bort.

Som alternativ när prestanda är avgörande kan vi använda `String#tr` som utför teckenersättning och `String#squeeze` som reducerar duplicerade tecken.

## See Also:
- Ruby's officiella dokumentation för [String#gsub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub) och [String#delete](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- Tutorial om reguljära uttryck i Ruby: [Rubular](http://rubular.com/)
