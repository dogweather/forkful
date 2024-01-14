---
title:                "Arduino: המרת מחרוזת לאותיות קטנות"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Why (למה): ארדואינו היא אחת מהפלטפורמות הפופולריות ביותר לתכנות הצמת האנלוגי הקשתי (מיקרו-כספי) וכוללת מגוון רחב של יכולות. אחת הפונקציות החשובות ביותר שמציע היא יכולת להמיר מחרוזת לאותיות קטנות. זה עשוי להיות שימושי במקרים שבהם אתה צריך לעבד בצורה ייעודית את הנתונים שבנתוני האינפוט.

How To (כיצד לעשות): להמרת מחרוזת לאותיות קטנות בארדואינו ניתן לבצע באמצעות פונקציית "toLowerCase". לדוגמה, נניח שרצינו להמיר את המחרוזת "ARDUINO" לאותיות קטנות, נוכל לכתוב את הקוד הבא בתוך "```"

ArduinoString text = "ARDUINO";
text.toLowerCase();
Serial.println(text);

פלט:
arduino

Deep Dive (מעמיקים): בתמונה הרחבה, נדון בצורת החישוב של הפונקציה "toLowerCase" בארדואינו. כאשר אנחנו משתמשים בפונקציה זו, היא יוצרת עותק חדש של המחרוזת ומחליפה את כל האותיות בתוכו לאותיות קטנות. זה נעשה על ידי שימוש במתודות (בתוך הפונקציה) שמזינות את האותיות הגדולות עם אותיות קטנות באמצעות קוד אסקי. כאשר הפונקציה מסתיימת, העותק החדש נמחק מהזיכרון והמחרוזת המקורית נשארת כפי שהייתה.

See Also (ראו גם):
- תיעוד רשמי של פונקציית toLowerCase בארדואינו: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/
- מדריך לתכנות בארדואינו למתחילים: https://www.arduino.cc/en/Guide/Introduction