---
title:                "עבודה עם csv"
html_title:           "Bash: עבודה עם csv"
simple_title:         "עבודה עם csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
עבודה עם CSV היא תהליך של עבודה עם קבצי נתונים שנמצאים בתבנית של טבלאות, בהם כל עמודה היא נתון וכל שורה מייצגת רשומה שונה. תהליך זה נפוץ מאוד בעולם התכנות כיוון שמאפשר לנו לארגן ולטפל בנתונים בצורה מהירה ויעילה.

## איך לעבוד עם CSV:
```Bash
# יצירת קובץ CSV חדש
echo "שם, מספר טלפון, אימייל" > contacts.csv

# הוספת רשומה חדשה
echo "גלית, 0501234567, galit@example.com" >> contacts.csv

# הצגת נתונים מהקובץ
cat contacts.csv 

# סינון לפי עמודה
cut -d "," -f 1 contacts.csv # יחזיר את עמודת השם

# שינוי ערכים בעמודת מסוימת
sed -i 's/מספר טלפון/טלפון/g' contacts.csv # יחליף את האותיות "מספר טלפון" במילה "טלפון"
```
תוצאה:
```
שם,טלפון,אימייל
גלית,0501234567,galit@example.com
```

## טיפול מתקדם בנתונים מקובץ CSV:
בכדי לעבוד ביעילות עם קבצי CSV ניתן להשתמש בכלים נוספים כמו סקריפטים או פקודות אחרות בשפות תכנות אחרות. חלק מהאלטרנטיבות שיכולות לסייע לנו בעבודה עם CSV הן: Excel, R, Python וכו'.

כאשר אנחנו עובדים עם קבצי CSV עם Bash נמנעים מהשימוש בכלים נוספים כיוון שעקביות השימוש בכלים מקטינה את סיכויינו לגרועות עם המידע. ניתן להשתמש בשפת Bash כדי להפעיל פקודות ישירות על קבצי CSV, כגון סינון ושינוי ערכים בקלות.

## ראו גם:
https://www.linuxjournal.com/content/working-csv-files-in-bash
http://tldp.org/LDP/abs/html/internalvariables.html
https://ryanstutorials.net/bash-scripting-tutorial/bash-variables.php