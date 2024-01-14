---
title:                "Ruby: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

למה: רק 1-2 משפטים שמסבירים *למה* מישהו יעסוק בחישוב תאריך בעתיד או בעבר.

כיצד לעבוד עם תאריכים ברובי:

בתחילה ניתן ליצור משתנה שמכיל את התאריך הנוכחי, על ידי השתמשות בפונקציית `Time.now`. לדוגמה:

```ruby
current_date = Time.now
puts current_date
```

כעת, בכדי לחשב תאריך בעתיד או בעבר, ישנן כמה אפשרויות. אתם יכולים להוסיף ימים, שעות, דקות או שניות לתאריך הקיים, או להשתמש בפונקציות כמו `next_week`, `next_day` וכו'. לדוגמה:

```ruby
# חישוב תאריך עוד שני ימים מהיום
future_date = current_date + 2.days
puts future_date

# חישוב תאריך לפני שבוע
past_date = current_date - 1.week
puts past_date

# חישוב תאריך לפני חודש
past_month = current_date.prev_month
puts past_month
```

ניתן גם ליצור תאריך מסוים בצורה ישירה, על ידי שימוש בפונקציית `Time.new` וציון היום, החודש והשנה. לדוגמה:

```ruby
# חישוב תאריך ב-15 לדצמבר 2021
future_date = Time.new(2021, 12, 15)
puts future_date
```

דילוג עמוק:

ברובי קיימות פונקציות מובנות רבות שמאפשרות לעבוד עם תאריכים בצורה נוחה וקריאה. בנוסף, ישנם גם ספריות נוספות שניתן להתקין ולהשתמש בהן כדי להעשיר את היכולות הקיימות. ישנן גם כמה טיפים וטריקים שיכולים לסייע בעבודה עם תאריכים ברובי. כדי ללמוד עוד, מומלץ לקרוא על הנושא בעומק יותר ולחפש מידע ברשת.

ראו גם:

- [העבודה עם תאריכים ברובי](https://developer.ibm.com/languages/ruby/articles/ruby-date-and-time/)
- [מדריך לתאריכים וזמנים ברוב