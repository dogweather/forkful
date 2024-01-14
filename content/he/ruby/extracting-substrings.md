---
title:    "Ruby: ניצול תת־מחרוזות"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## למה

למה ייתכן שתרצו למצוא חתיכת מחרוזת בתוך חריגה? אחת הסיבות העיקריות היא כדי לעבור על המחרוזת הראשית ולהחזיר מידע מדויק מתחתיה. כך ניתן לבנות קוד מדויק ויעיל יותר.

## איך לעשות זאת

```Ruby
# ניתן להשתמש בפונקצייה square brackets כדי למצוא תת-מחרוזת:
"I love Ruby programming.".index("Ruby") # => 7

# אם נרצה למצוא את המילה שלפני "programming":
"I love Ruby programming.".index(" programming.") # => 12

# אם נרצה לבדוק אם חתיכת המחרוזת קיימת בחריגה:
"I love Ruby programming.".include?("programming") # => true

# למצוא מילנים מתחילת החריגה:
"I love Ruby programming.".scan(/[A-Z][a-z]*/)[2] # => "Ruby"
```

## טיפים מעמיקים

- אם אתם מחפשים לחתוך חריגה לפי אורך מסוים, ניתן להשתמש בפונקציית `slice` או `slice!`.

- ניתן להשתמש בפונקציות נוספות כגון `start_with?` ו- `end_with?` כדי לבדוק את תחילת וסיום החריגה באופן מדויק.

- כדי לבדוק את מיקום החריגה בתוך מחרוזת אחרת, ניתן להשתמש בפונקציית `match` ולהכניס את החריגה כברירת המחדל.

## ראו גם

- [תיעוד על פונקציית `index` ו-`include?`](https://ruby-doc.org/core-2.6.6/String.html#method-i-index)
- [ויקיפדיה על חריגה](https://he.wikipedia.org/wiki/%D7%97%D7%A8%D7%99%D7%92%D7%94)
- [תיעוד על פונקציות `slice` ו-`slice!`](https://ruby-doc.org/core-2.6.6/String.html#method-i-slice)
- [תיעוד על פונקציות `start_with?` ו-`end_with?`](https://ruby-doc.org/core-2.6.6/String.html#method-i-start_with-3F)
- [תיעוד על פונקציית `match`](https://ruby-doc.org/core-2.6.6/String.html#method-i-match)