---
title:                "חילוץ תת-מחרוזות"
html_title:           "Arduino: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
תחשוב על הטקסט הארוך הבא: "20010607", במה ניתן להשתמש בו? מפרידים את הטקסט לפי ימים, חודשים ושנים לפי הצורך. הוכח שבכך ניתן לשפר את הסידור והקריאות של הטקסט, ולכן תוכנמים משתמשים בטכניקת חילוץ תת-מחרוזת.

## כיצד לעשות:
כאשר משתמשים בטכניקת חילוץ תת-מחרוזת ב-```Arduino```, ניתן להשתמש בפונקציית ```substring()```. ניתן להגדיר את המחרוזת המקורית ואת מיקום התת-מחרוזת המבוקשת באמצעות המשתנים הבאים: ```string.substring(start, end)```. לדוגמא, אם נרצה לחלץ את החודש מהטקסט שלנו, נשתמש בשורת קוד הבאה: ```string.substring(4, 6)```. הפלט שלנו יהיה "01", כלומר השני תווים הנמצאים בטקסט המקורי מהאינדקס 4 עד האינדקס 6.

## מכנס - המעמקים:
בעולם התכנות, חילוץ תת-מחרוזת הוא טכניקה נפוצה ושימושית. ניתן למצוא מגוון רחב של פונקציות חילוץ תת-מחרוזת בשפות תכנות שונות, לכן כדאי ללמוד את השימוש בטכניקה זו. בנוסף, ישנן גם פתרונות אחרים כמו לשנות את סוג המשתנה לתת-מחרוזת ולחלץ את הערכים בכך. חשוב לדעת שהטכניקה המתאימה תלויה ביישום ובטיב הקוד שלכם.

## ראו גם:
למידע נוסף על טכניקת חילוץ תת-מחרוזת ב-```Arduino```, ניתן להתייעץ עם המסמכים הבאים:

- [Official Arduino documentation on substring()](https://www.arduino.cc/en/Reference/StringSubstring)
- [Tutorial on extracting substrings in Arduino by Robojax](https://robojax.com/learn/tutorial/arduinoTutorial63_substrin/index.php)
- [Arduino String library reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)