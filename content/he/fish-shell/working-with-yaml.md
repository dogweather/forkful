---
title:                "Fish Shell: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

עבודה עם YAML נחשבת לכלי חשוב בעולם התכנות, המאפשר לכתב ולנהל פורמט טקסט קל ונגיש לאחסון נתונים. יתר על כן, תבניות YAML רחבות נהפכו למקור חזק להגדרת נתונים באפליקציות מגוונות. אם אתה מעוניין ללמוד כיצד לעבוד עם YAML בפיש של פיש, קרא את המדריך הזה ותגלה כמה פשוט זה!

## איך לעבוד עם YAML בפיש של פיש

YAML הוא פורמט טקסט פרוטוקולי נפוץ המאפשר לנו לארגן נתונים בצורה נגישה. עבור מתכנתים היכרות עם התחביר של YAML הוא דבר חשוב, שכן הם יתבקשו להשתמש בכלי זה מדי פעם. הנה דוגמא פשוטה של גישה להגדרת מחרוזת קטנה בקובץ YAML:

```fish shell
# ייבוא הספרייה yaml
$ source (dirname (status argv[0]))/yaml.fish

# יצירת קובץ ריק בשם "config.yml"
$ touch config.yml

# הגדרת פריט בטקסט YAML
$ echo "pet: fish" > config.yml

# קריאת הפריט והדפסת התוכן שלה
$ echo (yaml keys get config.yml)
```

מתוך זה נוכל לראות את הפלט הבא:

```output
pet: fish
```

ישנם עוד רבים שיטות לעבוד עם YAML בפיש של פיש, ואלו נמצאים במדריך זה.

## נכנסים לעומק

למען הוספת המון פירטים, נלמד כיצד ליצור, לקרוא ולערוך קבצי YAML בפיש של פיש. אנחנו נציג פעולות שכמעט כל מתכנת עוסק בתמיד יושם.

יצירת פריט נפתח את YAML התיק המבוקש, ויציג את שם קובץ הפלט עבור התוכן הזה. אתה רק צריך להכניס את הפעולה לקוד התוכנית שס