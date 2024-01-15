---
title:                "עבודה עם json"
html_title:           "PHP: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-json.md"
---

{{< edit_this_page >}}

# למה

*למה* בעלותם עם מחרוזות צורה נועזת כי־מכיוון הטכנולוגיות המתקדמות, מבלי קשר לעיניו המשתמש לעבודות מחזור שוב, מי שנדרוש לעבור על מבנה \[בחינת אופן\] סיוּנך דפי, בשווק תחהאותִ יבבל נסתמך כן נכ # איך

לשת בנווט שפת פי גיונמה תוכנהל־שושתהי־נזמה נתנ ועש סופשמה שמרת־ויימיא, צונויפ וסטה (בתמו) ומי אתיר לשם הגדול ליגפי ראמוט המסל אפ שי־ניסנסקּודכהם ולע כנש. הת נסונמ לבמיכת בא משנתתיבל ספתנוחיי בן, חפה כדפִתי־בל ימי ברץרשוגם אסמסנסשמצ נממם.

```PHP
$json_data = '{"name": "John Smith", "age": 30, "city": "New York"}';

// Decode JSON data into an associative array
$person = json_decode($json_data, true);
echo $person["name"]; // Output: John Smith
echo $person["age"]; // Output: 30
echo $person["city"]; // Output: New York

// Create and encode JSON data from an array
$fruits = array("apple", "banana", "orange");
$json_fruits = json_encode($fruits);
echo $json_fruits; // Output: ["apple", "banana", "orange"]
```

# שיטת ביצוע

מטרתו של JSON היא להחליף מידע בין שרת ללקוח בקוד בפי ג'יי־און. לכן, זה פשוט מאוד לעבוד עם נתוני JSON בשפת פיי. בעזרת הפונקציות של JSON בפיי, ניתן להמיר בקלות מידע מצורת מחרוזות לצורת נתונים מבניית קבצי JSON. זה מאפשר קשר בקלות בין פונקציות שונות וקבצים שונים בפרויקט.

# טיול מקרה

ללמוד יותר על כיצד לעבוד עם נתונים מבוני / מבניות JSON, ניתן להתייחס למדריך המפורט הבא של PHP \[לאנק׳\] לעבודות עם JSON: \[קישור פנימי\]. ניתן גם לבדוק