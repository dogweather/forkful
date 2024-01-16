---
title:                "עבודה עם json"
html_title:           "Fish Shell: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

(Hebrew Translation):

## למה

נתחיל בשאלה הכי חשובה - למה להתעסק עם JSON ולמה כדאי ללמוד כיצד לעבוד איתו מתוך ה- Fish Shell (גירסה הנוכחית)?

 JSON הוא שפת תכנות נפוצה מאוד ונמצאת בשימוש רב בפיתוח תוכניות ואתרים. הוא מאפשר יצירת מידע בפורמט נוח וקריא ומיועד להעברת נתונים בין מכשירים ואפליקציות שונות. כל נתונים שמתקבלים כדורשים להיות מאורגנים בפורמט JSON, וכך ניתן לקלוטם ולעבדם בצורה נוחה ויעילה.

## איך לפעול עם JSON ב- Fish Shell

לעבוד עם נתוני JSON ב- Fish Shell פשוט וקל כמו בכל שפת תכנות אחרת. באמצעות הפקודות המתאימות ניתן ליצור, לקרוא ולעדכן נתונים בפורמט זה. הנה כמה דוגמאות קצרות כדי להדגים את השימוש בפקודות הנכונות:

```Fish Shell
# יצירת אובייקט JSON
set person '{"name": "John", "age": 30}'

# קריאה של ערך מתוך אובייקט
echo $person.name

# עדכון ערך באובייקט
set person.age 35

# הדפסת אובייקט כטקסט
echo $person
```

הפקודות הנ"ל יכולות לשמש ככלים חזקים לעבוד עם JSON בפיתוח תוכניות ותכנות בפרויקטים שונים.

## ירידה מעומק ל- JSON

בנוסף לפקודות הבסיסיות שהיא מציעה, Fish Shell מציעה גם כמה פקודות עוצמתיות יותר לעבודה עם JSON. למשל, פקודת `jq` היא אחת מהכלים הנפוצים ביותר לפעילות עם JSON ומאפשרת ביצוע עקבי של מתודות ופקודות לנתונים.

עוד פקודה חזקה ו