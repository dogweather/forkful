---
date: 2024-01-20 17:48:42.171340-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05E8\u05D5\u05D1\u05D9 \u05E4\u05D9\
  \u05E8\u05D5\u05E9\u05D4 \u05DC\u05E1\u05E4\u05D5\u05E8 \u05D0\u05EA \u05DE\u05E1\
  \u05E4\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05D4. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E7\u05DC\u05D8, \u05DC\
  \u05D0\u05DE\u05EA \u05DE\u05D9\u05D3\u05E2, \u05D5\u05DC\u05D1\u05E6\u05E2 \u05E4\
  \u05E2\u05D5\u05DC\u05D5\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\
  \u05D8."
lastmod: '2024-03-13T22:44:40.185848-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05E8\u05D5\u05D1\u05D9 \u05E4\u05D9\u05E8\
  \u05D5\u05E9\u05D4 \u05DC\u05E1\u05E4\u05D5\u05E8 \u05D0\u05EA \u05DE\u05E1\u05E4\
  \u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\u05D4. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E7\u05DC\u05D8, \u05DC\u05D0\
  \u05DE\u05EA \u05DE\u05D9\u05D3\u05E2, \u05D5\u05DC\u05D1\u05E6\u05E2 \u05E4\u05E2\
  \u05D5\u05DC\u05D5\u05EA \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8\
  ."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת ברובי פירושה לספור את מספר התווים בה. מתכנתים צריכים את זה כדי לבדוק קלט, לאמת מידע, ולבצע פעולות עיבוד טקסט.

## איך לעשות:
```Ruby
str = "שלום עולם"
puts str.length
# => 9
puts str.size
# => 9
puts str.bytesize
# => 18
```
הפלט מראה אורך המחרוזת בתווים ובבתים. בעיברית, גודל בבתים יכול להיות גדול יותר כי תווים עבריים מיוצגים על ידי יותר מבית אחד.

## טבילה עמוקה:
במהלך השנים, שיטות לספירת תווים במחרוזות פותחו ושופרו. ברובי, `length` ו-`size` הם שני דרכים שקולות להשיג את אותו תוצאה. `bytesize`, לעומת זאת, מחזיר את מספר הבתים שמשמשים לייצג את המחרוזת, שיכול להיות שונה במיוחד עם קידוד UTF-8 כאשר מדובר בתווים לא אנגליים. זה משמעותי לעבודה עם מערכות שדורשות ניהול נכון של אחסון או זרימת נתונים כגון פרוטוקולי רשת.

האלטרנטיבה ל`length` ו-`size`, בעיקרן, היו מתודות נפוצות בגרסאות ישנות יותר של שפות. עם הזמן, רובי החדירה גישה יותר אינטואיטיבית של טיפול במחרוזות דרך מתודות זהות אלה.

בנוסף, רקע התכנות תומך ברעיון של פעולות נתונים, כמו מעריכת אורך של מחרוזת, ככלי בסיסי עבור אימות ועיבוד מידע. ברובי, לספירת התווים מתייחסים לקוד המחרוזת עצמו ולא להצגתו, דבר שמעניק גמישות בעבודה עם סוגי טקסט שונים.

## קישורים נוספים:
- [Ruby Docs for String Length](https://ruby-doc.org/core/String.html#method-i-length)
- [Understanding Bytes, Characters and Encodings](https://kunststube.net/encoding/)
