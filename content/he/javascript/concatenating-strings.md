---
title:                "שרשור מחרוזות"
html_title:           "Javascript: שרשור מחרוזות"
simple_title:         "שרשור מחרוזות"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
לחיבור מחרוזות הוא פעולה נפוצה בקוד JavaScript שמשמשת לחיבור שתי מחרוזות יחד כדי ליצור מחרוזת אחת גדולה. תהליך זה שהוא גם ידוע בשם "concatenation" נעשה מבודד כדי להקל על העבודה עם מחרוזות בתוך הקוד וליצירת קוד יעיל יותר.

## איך לעשות:
לביצוע חיבור מחרוזות בקוד JavaScript תכתבו את המחרוזות הרצויות עם סימן פלוס (+) ביניהן. לדוגמה: 
```JavaScript 
let string1 = "שלום"; 
let string2 = "עולם"; 
let combinedString = string1 + string2; 
console.log(combinedString); // הפלט יהיה "שלוםעולם" 
```

## חקירה מעמיקה:
התהליך של חיבור מחרוזות נמצא כבר תחת שימוש ממזמן, והוא משמש כבסיס ליצירת פעולות יותר מתקדמות כגון פתיחת קריאת קבצים או יצירת פורמטים אחרים. ישנן מספר דרכים נוספות לחבור מחרוזות בקוד כגון שימוש בפונקציות כמו "Join" או שימוש במשתנה "template literals" שנקרא גם "template strings" שנועד להשמיע את קריאת הקוד. 

## ראה גם:
למידע נוסף על חיבור מחרוזות בקוד JavaScript ניתן לפנות למקורות המקוונים הבאים:
- "מדריך למתחילים בJavaScript": https://developer.mozilla.org/he/docs/Learn/JavaScript/First_steps/Strings
- "מחרוזות בפייתון": https://www.w3schools.com/jsref/jsref_concat_string.asp 
- "הדרך הכי פשוטה לחבור מחרוזות בשפת פייתון": https://www.tutorialsteacher.com/python/string-concatenation-in-python