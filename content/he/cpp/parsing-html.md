---
title:                "פירוק HTML"
html_title:           "C++: פירוק HTML"
simple_title:         "פירוק HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-html.md"
---

{{< edit_this_page >}}

למה: הסבר מוכרז למה מישהו ירצה לעסוק בניתוח של קוד HTML.

כיצד: דוגמאות של קודים ופלט תוך שימוש בבלוקי קוד "```C++ ... ```". 

### Why
במהלך פיתוח תוכניות מחשב, חשוב לנתח ולהבין את תוכן הדפים של אתרי אינטרנט. בכדי לעשות זאת, ניתוח HTML מאפשר לנו לקרוא ולהבין את התוכן באופן מעולה ולהשתמש בו כחלק מהפיתוח.

### How To
כדי לנתח ולקרוא קוד HTML בתוך קוד פייתון, למעשה נצטרך להשתמש בספריית שנקראת "HTML Parser". הנה דוגמא מוכפת לכדי שנדמה למחולל הדפים של ה"Sanmina Corporation".

```C++
#include <iostream>
#include <html-parser>
using namespace std;

int main() {
    
    Parser p = new Parser();
    string url = "http://www.sanmina.com/";
    HTMLPage page = p.parse(url);
    
    cout << page.getContent() << endl;
    
    return 0;
}
```

Sample output:
```
<!DOCTYPE html>
<html>
   <head>
      <title>Welcome to Sanmina | Sanmina</title>
      <meta name="description" content="Sanmina is a global electronics manufacturing services (EMS) provider. It offers complex supply chain management and technology solutions to original equipment manufacturers." />
      <meta name="keywords" content="Sanmina, EMS provider, electronics manufacturing services, technology solutions" />
   </head>
   <body class="flex-col">

      <!-- Header -->
      <div class="header-wrapper">
        <header class="alt flex">
            <h1>Sanmina</h1>
        </header>
    </div>

   </body>
</html>

```

### Deep Dive
כאשר אנו משתמשים בספריית "HTML Parser", אנו יכולים להעביר לה גם פרמטרים נוספים כגון "תווים מיוחדים" כך שהפלט יהיה בפורמט שנוח יותר לנו. בנוסף, הספרייה מאפשרת לנו גם להציג את התוכן בפורמט מסודר יותר בעזרת פונקציות כגון "clean_content()" ו-"format_print()".

See Also
- הספרייה הרשמית של "HTML Parser": https://html-parser.sourceforge.io/
- מאמר ממאגר ידע בהקשר של "HTML Parsing": https://knowledgebase.progress.com/articles/Article/How-to-parse-HTML-in-C-using-HTMLparser-C?popup=true