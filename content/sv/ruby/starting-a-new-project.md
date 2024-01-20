---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att starta ett nytt projekt handlar om att påbörja utvecklingen av en unik programvarulösning. Programmerare gör det för att lösa specifika problem, förbättra processer eller skapa nya tjänster.

## Så här gör du:

Innan vi börjar med något Ruby-projekt, se till att du har den senaste versionen av Ruby installerad. Använd sedan `mkdir` för att skapa en ny mapp för ditt projekt:

```Ruby
#I din terminal, skriv
mkdir my_new_project
```

För att skapa en ny Ruby-fil, typ `touch` följt av namnet på din fil med `.rb`-ändelsen:

```Ruby
# Skapa en ny Ruby-fil
touch my_new_project.rb
```

Nu kan du skapa och köra en enkel `Hello, World!` kod för att se till att allt fungerar:

```Ruby
#I din Ruby-fil, skriv
puts 'Hello, World!'

# Kör din Ruby-fil
ruby my_new_project.rb
```
I din terminal bör du se: 
```Ruby
Hello, World!
```

Grattis, du har nu skapat och kört ditt första Ruby-projekt!

## Djupdykning

Ruby startade på mitten av 1990-talet av Yukihiro "Matz" Matsumoto, med en filosofi om att programmering bör vara njutbar för utvecklaren. Alternativa språk till Ruby inkluderar Python och JavaScript, men Ruby har fördelar som dess läsbarhet och flexibilitet.

När det gäller att starta ett nytt projekt, kom ihåg att en bra programmeringspraxis är att organisera ditt projekt i mappstrukturen. Tänk på att inkludera mappar för dina bibliotek (`lib`), tester (`test` eller `spec`), dokumentation (`docs`), och andra binärer (`bin`).

## Se även

1. The Ruby Programming Language, O'Reilly Media: [link here](https://www.oreilly.com/library/view/the-ruby-programming/9780596516178/)
2. Ruby Documentation: [link here](https://www.ruby-lang.org/en/documentation/)
4. The Basics of Creating a Ruby Gem: [link here](https://guides.rubygems.org/make-your-own-gem/)