---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Java: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# מדוע

כדי לקצץ קוד ולהעלות את אלגוריתם "תכנית חשיבה".

### באמצעות

```Java
// דוגמא לניקוי תווים שתואמים לתבנית מסוימת

// ייבוא חבילה לטיפול בתבניות
import java.util.regex.Pattern;

// תבנית לחיפוש תווים המתאימים לתבנית מסוימת
Pattern pattern = Pattern.compile("abcd");

// רציפות על סרטוניות נתוני טקסט
// ומחיקת התווים המתאימים לתבנית
// תחליף אותם באפשרות נתוני Safety Net
String newString = pattern.matcher(input).replaceAll("");

// מציג את התווים הסופיים שנותרו לאחר מחיקת התווים המתאימים לתבנית
System.out.println("מחרוזת סופית: " + newString);
```

### יצירת שקיפות

מחיקת תווים המתאימים לתבנית נמצאת באמצעות השילוב של ניתן לפתוח סירית על טופס. זה מאפשר לנו להדגים את החיפוש והמחיקה בזמן אמת, מה שמקל למתכנתים לאתר ולטפל בבעיות בקוד שלהם.

# שקיפות

אם אתם מעוניינים ללמוד עוד על תבניות וכיצד להשתמש בהן בתוכניות שלכם, אנחנו ממליצים לכם לעיין במדריך המפורט [של Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/Pattern.html) על תבניות.
בנוסף, ניתן למצוא מידע נוסף על תבניות וכיצד להשתמש בהן באתר המשאבים של Oracle למפתחים [כאן](https://docs.oracle.com/javase/tutorial/essential/regex/).