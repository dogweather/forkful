---
title:                "שימוש במערכים אסוציאטיביים"
date:                  2024-01-30T19:11:56.467450-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במערכים אסוציאטיביים"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, הידועים כמפות בGo, מאפשרים לך לאחסן ולגשת לנתונים באמצעות זוגות מפתח-ערך. הם חיוניים לניהול אוספים שבהם אפשר לחפש ערכים במהירות על ידי מפתח ייחודי, מה שמפשט את התמשאות והשליפה של נתונים בתוכניות שלך.

## איך ל:

בGo, מפות פשוטות לשימוש. הנה מדריך פשוט להתחלה:

1. **הצהרה ואתחול מפות**

```Go
package main

import "fmt"

func main() {
    // אתחול מפה ריקה עם מפתחות מסוג מחרוזת וערכים מסוג שלם
    var scores map[string]int
    fmt.Println(scores) // מדפיס: map[]

    // הצהרה ואתחול של מפה לא ריקה
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // מדפיס: map[green:#00ff00 red:#ff0000]
}
```

2. **הוספה וגישה לאלמנטים**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // מדפיס: 5
}
```

3. **עיבור על מפות**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s goes %s\n", key, value)
    }
    // סדר הפלט עשוי להשתנות, שכן המפות אינן מבטיחות סדר.
}
```

4. **מחיקת אלמנטים**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // לפני מחיקה

    delete(meals, "lunch")
    fmt.Println(meals) // אחרי מחיקה
}
```

## צלילה עמוקה

המפות הוצגו בGo 1 ומספקות דרך מובנת לטפל במערכים אסוציאטיביים בצורה יעילה. בניגוד לחתיכות, שהן אוספים מסודרים, המפות הן לא מסודרות. זה אומר שסדר העיבור על אלמנטי המפה אינו מובטח להיות זהה בין ביצועים, פשרה בשביל היכולת שלה לטפל בזוגות מפתח-ערך בצורה דינמית וגמישה.

מתחת לכיפה, Go מיישמת מפות כטבלאות גיבוב, מה שמבטיח שמורכבות ממוצעת של פעולות גישה, הוספה, ומחיקה היא O(1), תחת רוב הנסיבות. עם זאת, כדאי לציין שהיעילות הזו עשויה להשתנות בהתבסס על גורמים כמו התנגשויות בגיבוב.

למקרים שבהם נדרש עיבור על מפתחות בסדר, כדאי לשקול לשלב בין מפות לחתיכות או לחקור חבילות צד שלישי המציעות מבני נתונים נוספים כמו מפות מסודרות או עצים. למרות הגבלותיהן, מפות Go הן כלי חזק וחיוני לתרחישים רבים בתכנות.