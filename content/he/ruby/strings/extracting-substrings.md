---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:46:34.472563-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Ruby \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05D7\u05DC\u05E5 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D1\u05EA\u05EA\u05D9-\u05DE\u05D9\u05EA\u05E8 \u05D1\u05E6\u05D5\
  \u05E8\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4. \u05D1\u05D5\u05D0\u05D5 \u05E0\u05D9\
  \u05DB\u05E0\u05E1 \u05D1\u05E2\u05E0\u05D9\u05D9\u05E0\u05D9\u05DD."
lastmod: '2024-04-04T00:27:13.324263-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05D7\u05DC\u05E5 \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D1\u05EA\u05EA\u05D9-\u05DE\u05D9\u05EA\
  \u05E8 \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## איך לעשות:
Ruby מאפשרת לחלץ מחרוזות בתתי-מיתר בצורה פשוטה. בואו ניכנס בעניינים:

```Ruby
str = "Hello, Ruby World!"

# שיטה 1: שימוש במקומות מערך
substring = str[7, 4] # "Ruby"
puts substring

# שיטה 2: שימוש במתודת slice
slice = str.slice(7, 4) # "Ruby"
puts slice

# שיטה 3: ביטויים רגולריים
match = str[/[Rr]uby/] # "Ruby"
puts match

# שיטה 4: split וגישה למערך
split_array = str.split # מחלק ברירת מחדל בפסיקיות
picked_word = split_array[2] # "World!"
puts picked_word
```

תצוגה מקדימה של כל קטע תהיה "Ruby", "Ruby", "Ruby", "World!" בהתאם.

## עיון נוסף
בעבר, לחלץ מחרוזות תתי-מיתר היה תהליך יותר מפורט. Ruby התפתחה, אך כיום, יש לך מתודות וביטויים רגולריים זמינים לשימוש.

הנה מה שקורה מאחורי הקלעים: `[7, 4]` אומר התחל בתו השביעי ותפוס את ארבעת התווים הבאים. `slice` זו דרך מתודית לומר את אותו דבר. באמצעות ביטויים רגולריים, `/[Rr]uby/` זה כמו לומר, "תשיג לי 'Ruby' או 'ruby', אשר ימצא לפני." `split` קוטע את המחרוזת למערך בכל רווח, ו`[2]` בוחר את המילה השלישית—זכור, מערכים מתחילים מאפס.

אלטרנטיבות? בטח, ל-Ruby יש. `partition`, `rpartition`, ו`match` יכולים גם לשחק כאן. לכל אחד יש את המקרה שלו אך לדעת את `.slice` וביטויים רגולריים מכסה את רוב הבסיסים.

בקצרה: חילוץ מחרוזות תתי-מיתר הוא על מניפולציה מדויקת של טקסט. הכלי הנכון אומר קוד נקי ויעיל.

## ראה גם
- מסמכי Ruby על מחרוזת: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- ביטויים רגולריים ב-Ruby: [ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- מדריך סגנון Ruby על מחרוזות: [rubystyle.guide/#strings](https://rubystyle.guide/#strings)
