---
title:    "Ruby: המרת מחרוזת לאותיות קטנות"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# למה:

תוכנםקי Ruby מציע את היכולת להמיר מחרוזת לאותיות מינוסקוליות בקלות ובפשטות, מה שיכול להיות שימושי למגוון רחב של מטרות. לדוגמה, אם אתה מטפל בקלט משתמש ואתה רוצה לוודא שהמתנהג הנכון שלו הוא לא תלוי באותיות, אז יכול להיות שימושי להמיר את המחרוזת לאותיות מינוסקוליות כדי להבטיח תוצאות נכונות.

# איך לעשות זאת:

```Ruby
מחרוזת = "זוהי מחרוזת עם אותיות רוגיות"
puts מחרוזת.downcase
# תוצאה: זוהי מחרוזת עם אותיות רוגיות
```

בדוגמה זו, אנו משתמשים בשיטה .downcase כדי להמיר את המחרוזת לאותיות מינוסקוליות. אתה יכול גם לשמור את התוצאה במשתנה חדש אם תרצה.

# חפירה מעמוק:

השיטה .downcase מצויה גם כחלק ממבחר של שיטות שמנהלת את המרה נכונה של מחרוזות. אתה יכול למצוא מידע נוסף על השימוש בשיטות אלה ואיך ליישם אותן בקוד שלך באתר הרשמי של Ruby.

# ראה גם:

- https://www.ruby-lang.org/he/documentation/
- https://ruby-doc.org/core-2.7.3/String.html#method-i-downcase
- https://medium.com/@amlivingstone/transforming-strings-in-ruby-78761d9469c4#.829nuorwp