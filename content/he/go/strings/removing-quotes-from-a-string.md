---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:22.436203-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Go \u05DE\u05E6\u05D9\
  \u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05D2\u05D9\u05E9\u05D5\u05EA \u05DC\u05D4\
  \u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA, \u05D0\u05DA \u05D0\u05D7\u05EA \u05D4\u05D3\u05E8\u05DB\
  \u05D9\u05DD \u05D4\u05E4\u05E9\u05D5\u05D8\u05D5\u05EA \u05D1\u05D9\u05D5\u05EA\
  \u05E8 \u05D4\u05D9\u05D0 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D5\u05EA `Trim` \u05D5-`TrimFunc` \u05E9\u05DE\u05E1\
  \u05D5\u05E4\u05E7\u05D5\u05EA \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05D7\u05D1\
  \u05D9\u05DC\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.468196-06:00'
model: gpt-4-0125-preview
summary: "Go \u05DE\u05E6\u05D9\u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05D2\u05D9\u05E9\
  \u05D5\u05EA \u05DC\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\
  \u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA, \u05D0\u05DA \u05D0\u05D7\u05EA\
  \ \u05D4\u05D3\u05E8\u05DB\u05D9\u05DD \u05D4\u05E4\u05E9\u05D5\u05D8\u05D5\u05EA\
  \ \u05D1\u05D9\u05D5\u05EA\u05E8 \u05D4\u05D9\u05D0 \u05DC\u05D4\u05E9\u05EA\u05DE\
  \u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA `Trim` \u05D5-`TrimFunc`\
  \ \u05E9\u05DE\u05E1\u05D5\u05E4\u05E7\u05D5\u05EA \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05D4\u05D7\u05D1\u05D9\u05DC\u05D4 `strings`."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05EA\
  \u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## איך לעשות:
Go מציעה מספר גישות להסרת ציטוטים ממחרוזת, אך אחת הדרכים הפשוטות ביותר היא להשתמש בפונקציות `Trim` ו-`TrimFunc` שמסופקות על ידי החבילה `strings`. הנה איך לעשות זאת:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"This is a 'quoted' string"`

	// באמצעות strings.Trim להסרת ציטוטים ספציפיים
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("באמצעות strings.Trim:", unquoted)

	// גישה מותאמת אישית באמצעות strings.TrimFunc לבקרה רבה יותר
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("באמצעות strings.TrimFunc:", unquotedFunc)
}
```

דוגמה זו מדגימה שתי גישות להסרת ציטוטים כפולים (`"`) ויחיד (`'`). הפונקציה `strings.Trim` היא פשוטה יותר ועובדת היטב כאשר אתה יודע בדיוק אילו תווים להסיר. מאידך, `strings.TrimFunc` מספקת גמישות רבה יותר, מאפשרת לך לציין פונקציה מותאמת אישית להחלטה אילו תווים להסיר. פלט הדוגמה של הקוד לעיל הוא:

```
באמצעות strings.Trim: This is a 'quoted' string
באמצעות strings.TrimFunc: This is a 'quoted' string
```

שתי השיטות מסירות ביעילות את הציטוטים המובילים והסופיים מהמחרוזת.

## צלילה לעומק
הפונקציות `Trim` ו-`TrimFunc` מתוך החבילה `strings` הן חלק מהספרייה הסטנדרטית המקיפה של Go, שנועדה להציע יכולות מניפולציה על מחרוזות חזקות, עם זאת פשוטות, ללא הצורך בחבילות צד שלישי. הצורך ההיסטורי לטפל ולמנפל מחרוזות ביעילות נבע מהמיקוד הראשוני של Go על שרתי רשת ומנתחי נתונים, שם עיבוד מחרוזות הוא משימה נפוצה.

אחד היבט מובלט בפונקציות אלו הוא היישום שלהן מבוסס על רונים (הייצוג של Go לנקודת קוד יוניקוד). העיצוב הזה מאפשר להן לטפל בחלקות במחרוזות המכילות תווים מרובי בתים, וכך מתקבלת גישה למניפולציית מחרוזות הן חזקה והן נדיבה ביוניקוד.

למרות שהשימוש הישיר ב-`Trim` ו-`TrimFunc` להסרת ציטוטים הוא נוח ואידיומטי ב-Go, כדאי לציין שעבור משימות עיבוד מחרוזות מורכבות יותר (למשל, ציטוטים מקוננים, ציטוטים מוברחים), ביטויים רגולריים (דרך חבילת `regexp`) או ניתוח ידני עשויים לספק פתרונות טובים יותר. עם זאת, לחלופות אלו יש שיקולי סיבוכיות וביצועים. לכן, למשימת הסרת ציטוטים פשוטה, השיטות שהוצגו מספקות איזון טוב בין פשטות, ביצועים ופונקציונליות.
