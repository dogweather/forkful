---
title:    "C++: Sammanslagning av strängar"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Varför

Att sammanslå strängar, även kallat konkatinering, är en vanlig teknik inom C++ programmering. Det kan användas för att skapa mer dynamiska och anpassningsbara textutmatningar i ditt program.

## Hur man gör det

I C++ finns det två olika sätt att sammanslå strängar: med hjälp av betingade operationer (+=) eller med en särskild funktion som heter strängkonkatinering. Här är ett exempel på hur man kan använda båda metoderna:

```C++
//Betingad operation
string förnamn = "Lisa";
string efternamn = "Andersson";
förnamn += efternamn;
cout << förnamn << endl;

//Strängkonkatinering
string förnamn = "Lisa";
string efternamn = "Andersson";
string namn = förnamn.append(efternamn);
cout << namn << endl;
```

Det första exemplet använder betingad operation för att sammanslå strängarna "Lisa" och "Andersson" och lagra resultatet i variabeln "förnamn". Det andra exemplet använder strängkonkatinering för att göra samma sak, men lagrar resultatet i en ny variabel "namn". Båda kodexemplen ger samma utmatning: "LisaAndersson".

## Djupdykning

När man sammanslår strängar i C++ är det viktigt att förstå att de olika metoderna har olika egenskaper. Betingad operation lägger till strängar på slutet av en befintlig sträng medan strängkonkatinering faktiskt skapar en helt ny sträng. Detta kan påverka prestandan i ditt program, speciellt om du sammanslår strängar inuti en loop.

Det finns också vissa aspekter av sammansättning av strängar som man bör vara medveten om:

- Strängkonkatinering är effektivare när man behöver sammanslå flera strängar
- Det är viktigt att ha rätt datatyp på variablerna när man använder betingad operation, annars kan det leda till oförutsedda resultat
- Olika programmeringsplattformar kan ha olika sätt att hantera specialtecken och teckentabeller när man sammanslår strängar, se till att ha koll på detta om ditt program behöver skriva ut specialtecken

## Se också

Om du vill lära dig mer om sammanslagning av strängar i C++, kolla gärna på dessa länkar:

- [C++ String Concatenation Guide (engelska)](https://www.techiedelight.com/concatenate-strings-cpp/)
- [C++ String Concatenation Tutorial (engelska)](https://www.programiz.com/cpp-programming/string-concatenation)
- [C++ String Concatenation - All You Need to Know (engelska)](https://www.educba.com/string-concatenation-in-cpp/)

Lycka till med din strängkonkatinering!