---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? - מה ולמה?
כתיבה לטעות סטנדרטית (standard error) משמשת לדיווח על שגיאות בתוך תוכנה. תכניתנים עושים זאת כדי לנתב הודעות שגיאה לערוץ נפרד מפלט רגיל, מה שמאפשר ניתוח וניטור יעילים יותר.

## How to: - איך לעשות:
```Ruby
# הדפסה רגילה לפלט הסטנדארטי
puts "Hello, this is standard output."

# כתיבה לטעות הסטנדרטית
$stderr.puts "Warning, this is a standard error!"

#דוגמא לפלט:
# Hello, this is standard output.
# Warning, this is a standard error!
```

## Deep Dive - צלילה עמוקה:
הכתיבה לטעות הסטנדרטית ב-Ruby משתמשת בעולם היוניקס כקונבנציה מאז שנות ה-70. אלטרנטיבות כוללות יומנים (logging) או כתיבה לקבצים. `$stderr` היא גלובלית וניתן לשנות את המיקום שלה, אבל ברוב המקרים יש להשתמש בה כפי שהיא.

## See Also - ראה גם:
- [Ruby's IO class](https://ruby-doc.org/core-3.0.0/IO.html)
- [The Unix standard streams article on Wikipedia](https://en.wikipedia.org/wiki/Standard_streams)
