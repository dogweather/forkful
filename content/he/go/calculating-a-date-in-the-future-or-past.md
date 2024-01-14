---
title:                "Go: חישוב תאריך בעתיד או בעבר"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# למה?
בשפת Go ישנו כמה טכניקות שימושיות לחישוב תאריך בעתיד או בעבר. ייתכן שלכתובתחילה לא תבין מדוע תצטרך לעשות זאת, אבל עם הזמן תגלה שישנם מספר מצבים בהם זה יכול להיות מאוד שימושי.

# כיצד לבצע?
מטרת הגול שלנו היא לחשב תאריך בעתיד או בעבר. נשתמש בפונקציות time.Parse ו-time.Format כדי להמיר את התאריך לפורמט המבוקש. הנה דוגמאות קוד עם פלט רלוונטי:

```Go
// חישוב תאריך בעתיד
future := "2022-01-01"
t, _ := time.Parse("2006-01-02", future)
fmt.Println("תאריך בעתיד:", t.Format("Jan 2006"))

// חישוב תאריך בעבר
past := "1990-05-03"
t, _ := time.Parse("2006-01-02", past)
fmt.Println("תאריך בעבר:", t.Format("Jan 2006"))
```

# חפירה עמוקה
כעת שנכיר את הטכניקות הנדרשות, נוכל לחקור עמוק יותר בשימוש בפונקציות time.Parse ו-time.Format. נבין כיצד בדיוק הן עובדות וכיצד ניתן להתאים אותן לצרכים שלנו. בנוסף, נוכל ללמוד על הפונקציות הנוספות שמציגות פורמט של תאריך מסוים וכיצד להשתמש בהן בצורה נכונה.

# ראו גם
- המדריך הרשמי של Go לשימוש בפונקציות time.Parse ו-time.Format: https://golang.org/pkg/time/#Parse
- תיעוד לפונקציות נוספות של Go בנושאים קשורים: https://golang.org/pkg/time/