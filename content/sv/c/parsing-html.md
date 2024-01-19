---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-html.md"
---

{{< edit_this_page >}}

# Analys av HTML i C programmering (Parsing HTML in C programming)

## Vad och Varför? (What & Why?) 
Att analysera HTML innebär att omvandla HTML-kod till en annan struktur som är lättare att använda för programmeraren. Vi gör detta för att effektivt extrahera, navigera och manipulera webbdata.

## Hur till: (How to:)
Här kommer vi att använda Gumbo parser som är en implementering av HTML5 parsingsalgoritmen för C.

Först installerar vi Gumbo parser:

```C
sudo apt-get install libgumbo-dev
```

Sedan skriv följande kod för att analysera HTML:

```C
#include <stdio.h>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
  if (node->type != GUMBO_NODE_ELEMENT) {
    return;
  }
  GumboAttribute* href;
  if (node->v.element.tag == GUMBO_TAG_A &&
    (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
    printf("%s\n", href->value);
  }

  GumboVector* children = &node->v.element.children;
  for (unsigned int i = 0; i < children->length; ++i) {
    search_for_links(children->data[i]);
  }
}

int main() {
  GumboOutput* output = gumbo_parse("<a href='http://google.com'>Google</a>");
  search_for_links(output->root);
  gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

När du kör programmet får du följande utmatning:

```C
http://google.com
```

## Djup Dykning: (Deep Dive)
Gumbo parser är en ganska ny parsingsmotor, frisläppt av Google 2013. Historiskt sett har det varit en utmanande process att analysera HTML effektivt och korrekt. 

Alternativ till Gumbo parser är bland annat Htmlcxx och Myhtml, men Gumbo tenderar att vara det mest populära verktyget på grund av dess kompatibilitet med HTML5.

Ett intressant detalj om att implementera HTML-parsern i C är möjligheten att skriva C-kod som är både effektiv och kompakt. Eftersom C är ett lägre nivå språk än till exempel Python, kan vi uppleva prestandafördelar genom att använda det.

## Se Även: (See Also)
1. Gumbo Parser Dokumentation - https://github.com/google/gumbo-parser
2. Htmlcxx Dokumentation - https://github.com/htmlcxx/htmlcxx
3. Myhtml Dokumentation - https://github.com/lexborisov/myhtml