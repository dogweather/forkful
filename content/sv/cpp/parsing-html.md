---
title:                "Analysera html"
html_title:           "C++: Analysera html"
simple_title:         "Analysera html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsra HTML är processen att återställa strukturen på en webbsida från sin textbaserade kod till en mer läsbar form. Detta hjälper till att extrahera information från en webbsida och kan vara användbart för att skapa webbskrapare eller automatiserade processer.

## Hur man gör:

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <regex> // regex library for C++
using namespace std;

int main() {
  // Open HTML file for parsing
  ifstream file("index.html");
  string line;

  // Read each line of the file
  while(getline(file, line)) {
    // Use regex to find tags and their attributes
    regex tag_regex("<\\w+>|<\\/\\w+>");
    regex attr_regex("\\w+=\"[^\"]+\"");

    // Find and output tags
    smatch tag_match;
    while (regex_search(line, tag_match, tag_regex)) {
      cout << "Tag: " << tag_match[0] << endl;
      // Find and output attributes
      smatch attr_match;
      while (regex_search(tag_match[0].str(), attr_match, attr_regex)) {
        cout << "Attribute: " << attr_match[0] << endl;
      }
      // Remove the found tag from the line
      line = tag_match.suffix();
    }
  }
}
```

Output:

```
Tag: <html>
Tag: <head>
Tag: <title>
Attribute: title="Min Websida"
Tag: </title>
Tag: </head>
Tag: <body>
Tag: <h1>
Attribute: class="rubrik"
Tag: </h1>
```

## Djupgående:

Att parsra HTML har blivit ett viktigt verktyg för webbprogrammerare, särskilt med den ökande användningen av automatiserade processer och webbskrapare. Historiskt sett har det funnits många olika sätt att parse HTML, inklusive användning av parserbibliotek som "libxml" eller "clang", men regex har blivit alltmer populärt som ett enklare och snabbare alternativ.

För de som vill ha mer kontroll över parsingprocessen kan man också implementera en egen parser från grunden. Detta kan vara användbart för komplicerade och specialiserade parseringar, men det kräver mer kunskap och tid för utveckling.

## Se även:

- [C++ Regex Library](https://www.cplusplus.com/reference/regex/)
- [LibXML](http://xmlsoft.org/)
- [Clang](https://clang.org/)