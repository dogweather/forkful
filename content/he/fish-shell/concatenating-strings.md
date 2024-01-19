---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# "## מה ולמה?"

תרגול מילולים הוא הערכה של שני או יותר מחרוזות יחד. מתכנתים משתמשים בתהליך זה כדי לאחד מידע, לעשות אותו יותר קריא או להביא אותו לתבנית מסוימת.

# "## איך לעשות:"

ב- Fish Shell, אפשר לחבר מחרוזות בצורה מאוד פשוטה. הנה דוגמה:

```fish
set str1 "שלום, "
set str2 "עולם!"
echo $str1$str2
```

פלט:

```fish
שלום, עולם!
```

# "## הצצה עמוקה יותר:"

תהליך מילול המחרוזות הוא חלק מהיסטוריה של שפות תכנות רבות, נמשך מאז הימים הראשונים של שפת ה- COBOL. 

אלטרנטיבות עשויות לכלול שימוש בפעולות מיוחדות למילול כדי להוסיף תווים על גבי מחרוזת, אך ב-Fish Shell אפשר פשוט להשתמש בסימן '\' בשל שיטת הקשר הלינארית המקוננת. 

בקצה מילול, פונקציה פנימית מחברת את המחרוזות יחד. "מילול" הוא סוג של פונקציית אחזקה שמחזיקה בכמה מחרוזות במקביל ומחברת אותן יחד.

# "## ראה גם:"

- [תיעוד Fish Shell ל- String](https://fishshell.com/docs/3.1/commands.html#string)
- [מנהג מאמר על מילול בשפת תכנות](http://www.bagrit.com/concatenation-in-programming-languages/)
- [הסבר על פונקציות אחזקה](https://he.wikipedia.org/wiki/%D7%A4%D7%95%D7%A0%D7%A7%D7%A6%D7%99%D7%99%D7%AA_%D7%90%D7%97%D7%96%D7%A7%D7%94)