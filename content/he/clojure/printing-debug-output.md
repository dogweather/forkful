---
title:    "Clojure: הדפסת פלט מניפולציות שגיאה"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## למה
למה צריך להשתמש בפלט מחקר כאשר מתכנתים ב־Clojure? ישנם כמה סיבות שמצדיקות את השימוש בפלט מחקר. הראשונה היא כי זה מאפשר לנו לבדוק את הקוד ולאתר ולתקן שגיאות. השנייה היא שזה מעזר לנו להבין את זרימת הקוד ולבדוק שהמטרות שלנו מתקיימות בצורה נכונה.

## איך לעשות זאת
למתכנתים בפ־Clojure יש מספר אפשרויות להדפיס פלט מחקר. אחת האפשרויות הכי פשוטות היא להשתמש בפונקציית `println` כדי להדפיס את הערכים שאנחנו רוצים לבדוק. למשל:

```Clojure
(defn print-debug []
  (let [a 5
        b 10]
    (println "a =" a)
    (println "b =" b)))
```

בתוך הפונקציה `print-debug`, אנחנו משתמשים בסעיפים כדי להגדיר ערכים שאנחנו רוצים לבדוק. האחרון, אנחנו משתמשים בפונקציית `println` כדי להדפיס אותם למסך. כשנקרא לפונקציה הזו, נקבל את הפלט הבא:

```Clojure
a = 5
b = 10
```

הפלט הזה מציג לנו את ערכי המשתנים שנגדיר ובכך מסייע לנו לבדוק את הערכים שלנו במהלך הקוד.

## עיון מעמיק
אנחנו יכולים להשתמש בפלט מחקר גם כדי לעקוב אחר תהליכי הקוד ולאתר את החלקים שלא עובדים כראוי. למשל, אם יש לנו מספר פונקציות ששובצות אחת באחת, אנחנו יכולים להוסיף פלט מחקר בכל פונקציה כדי לבדוק את הערכים שלהן. כשנקיאל לפונקציות הללו, נוכל לקבל תמונה מפורטת יותר של זרימת הקוד ולאתר א