---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
simple_title:         "שימוש בביטויים רגולריים"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
Regular expressions, או regex, זה סט של תווים שמתארים דפוס בטקסט. תכנתים משתמשים בזה כדי לחפש, לתקן, או לנתח נתונים בצורה מהירה ונכונה.

## איך לעשות:
```Ruby
# חיפוש טקסט בחרוז
puts "hello world" =~ /world/  # ידפיס 6, כי המילה "world" מתחילה באינדקס 6

# תחליפי את כל המופעים של המילה "world" ל-"earth"
puts "hello world, world!".gsub(/world/, 'earth')  # ידפיס "hello earth, earth!"

# מצא את כל המספרים בטקסט
numbers = "There are 123 apples, 50 bananas, and 30 guavas".scan(/\d+/)
puts numbers  # ידפיס ["123", "50", "30"]
```

## עיון מעמיק:
Regex התפתח בשנות ה-50 וה-60 על ידי עבודתו של סטיבן קולעוק. חלופות כוללות מנועי חיפוש טקסט מובנים כמו `indexOf` או ספריות ניתוח טקסט כמו `StringScanner` ב-Ruby. פירוט המימוש ב-Ruby מבוסס על ספריה שמכונה Oniguruma.

## ראה גם:
- [Ruby Regular Expressions](https://ruby-doc.org/core-2.7.0/Regexp.html) - תיעוד ה-Regexp מ-Ruby.
- [Rubular](http://www.rubular.com/) - כלי לבדיקת regex בזמן אמת עבור Ruby.
- [Oniguruma](https://github.com/kkos/oniguruma) - ספריית Regex ש-Ruby משתמשת בה.
