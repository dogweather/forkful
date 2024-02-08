---
title:                "רפקטורינג"
aliases:
- he/elixir/refactoring.md
date:                  2024-01-26T01:18:45.054630-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא תהליך של שינוי מבנה של קוד קיים ללא שינוי הת comportamiento חיצוני שלו, עם המטרה לשפר תכונות שאינן פונקציונליות כמו קריאות ותחזוקה. מתכנתים עושים זאת כדי להפוך את הקוד לנקי יותר, קל יותר להבנה ויעיל יותר, מה שמקל על עדכונים עתידיים ומפחית את הסיכון לבאגים.

## איך לעשות:
בואו נסדר דוגמת Elixir נפוצה. נבצע ריפקטורינג לפונקציה `calculate_stats` שעושה יותר מדי על ידי שבירתה לחלקים קטנים יותר וניתנים לשימוש חוזר.

```elixir
defmodule Stats do
  # קוד מקורי, לפני ריפקטורינג
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # קוד מרופקטור
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# פלט לדוגמה
# לפני ריפקטורינג
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# אחרי ריפקטורינג
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
כפי שאתם רואים, הפלט נשאר זהה, אבל עכשיו יש לנו פונקציות מודולריות שניתן לנצל ולבדוק באופן עצמאי.

## צלילה עמוקה
ריפקטורינג אינו מושג חדש; הוא חלק בלתי נפרד מתכנות מהימים הראשונים של פיתוח תוכנה. יצירות חשובות, כמו "Refactoring: Improving the Design of Existing Code" מאת מרטין פאולר, מספקות תרגולים בסיסיים לריפקטורינג עם תובנות לגבי מתי ואיך ליישם אותם.

חלופות לריפקטורינג ידני כוללות כלים אוטומטיים לניתוח קוד, אשר יכולים להציע או אף לבצע ריפקטורינגים. עם זאת, כלים אוטומטיים לעיתים אינם מבינים את ההקשר המלא של הקוד ויכולים לפספס דקויות שמתקן אנושי ילכוד.

פרטי יישום ב-Elixir כוללים הבנת הפרדיגמה הפונקציונלית וניצול של התאמת דפוסים, סעיפי שמירה ואופרטור הצינור לכתיבת קוד ברור ותמציתי. למשל, ריפקטורינג לעיתים כולל המרה של פונקציות מסוג פקודתי מורכבות לפונקציות קטנות יותר, שניתנות להרכבה, העוקבות אחרי העדפת Elixir לאי-שינוי ופעולות ללא תופעות לוואי.

## ראו גם
למידע נוסף על טכניקות ריפקטורינג ספציפיות ל-Elixir:

- [המדריכים הרשמיים של Elixir](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" מאת מרטין פאולר](https://martinfowler.com/books/refactoring.html), לעקרונות כלליים שניתן ליישם על Elixir.
- [Credo, כלי ניתוח קוד סטטי ל-Elixir](https://github.com/rrrene/credo) שמעודד מתודות עבודה מומלצות.
- [מסלול Elixir ב-Exercism](https://exercism.org/tracks/elixir), לתרגולים מעשיים שלעיתים כרוכים בריפקטורינג.
