---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שמתאימים לדפוס הינה יכולת למחוק הופעות של תווים מוגדרים מתוך מחרוזת. תיכנתים ביצעים זאת כדי למנוע רעש נוסף במידע או להקל על ניתוח המידע.

## איך לפתוח:
להלן דוגמאות לקוד ולתוצאת הדפסה בתוך בלוקי קוד ```Clojure ... ```

```Clojure
(defn remove-char [str c] 
  (clojure.string/replace str (str c) ""))
```

בדוגמה זו, הפונקציה `remove-char` מקבלת מחרוזת ותו להסרה. באמצעות הפונקציה `clojure.string/replace`, אנו מחליפים את התו בתוך המחרוזת בתו ריק.

```Clojure
(remove-char "Hello, World!" "o")
```

הפקודה הזו תחזיר: "Hell, Wrld!".

## צלילה עמוקה
מחיקת תווים על פי דפוס הייתה חלק משפות התכנות מאז הגנרציה הראשונה שלהם.
אלטרנטיבה נוספת למחיקה היא השימוש בפונקציה `clojure.string/escape`. במקרה זה, אתה תהיה מחליף את התווים בתו אחר, במקום להסיר אותם.  
הפונקציה `clojure.string/replace` מיישמת גרסה פשוטה של אלגוריתם למחיקת תווים שמתאימים לדפוס, שמשתמשת בפונקציות עזר אחרות מתוך השפה כדי להשיג את המטרה שלה.


## ראה גם 
הקישורים הבאים מציעים מידע נוסף עם קשר לתחום הזה:
- API של Clojure למעבדת מחרוזות: https://clojuredocs.org/clojure.string
- גרסות אחרות של מחיקת תווים בשפות תכנות אחרות: https://stackabuse.com
- אלגוריתמים למחיקת תווים במחרוזת: https://www.wikihow.com/Delete-Characters-in-String