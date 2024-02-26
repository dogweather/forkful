---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:54.598495-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05E8\u05D5\u05D1\u05D9 \u05D4\u05DD \u05D3\u05E4\
  \u05D5\u05E1\u05D9\u05DD \u05D4\u05DE\u05E9\u05DE\u05E9\u05D9\u05DD \u05DC\u05D4\
  \u05EA\u05D0\u05DE\u05EA \u05E6\u05D9\u05E8\u05D5\u05E4\u05D9 \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05DE\u05D4 \u05E9\
  \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05E4\u05EA\u05D7\u05D9\u05DD \u05DC\
  \u05D7\u05E4\u05E9, \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D5\u05DC\u05D8\u05E4\
  \u05DC \u05D1\u05D8\u05E7\u05E1\u05D8 \u05D1\u05D9\u05E2\u05D9\u05DC\u05D5\u05EA\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\
  \u05DD\u2026"
lastmod: '2024-02-25T18:49:38.419682-07:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD (regex) \u05D1\u05E8\u05D5\u05D1\u05D9 \u05D4\u05DD \u05D3\u05E4\
  \u05D5\u05E1\u05D9\u05DD \u05D4\u05DE\u05E9\u05DE\u05E9\u05D9\u05DD \u05DC\u05D4\
  \u05EA\u05D0\u05DE\u05EA \u05E6\u05D9\u05E8\u05D5\u05E4\u05D9 \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05DE\u05D4 \u05E9\
  \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05DE\u05E4\u05EA\u05D7\u05D9\u05DD \u05DC\
  \u05D7\u05E4\u05E9, \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D5\u05DC\u05D8\u05E4\
  \u05DC \u05D1\u05D8\u05E7\u05E1\u05D8 \u05D1\u05D9\u05E2\u05D9\u05DC\u05D5\u05EA\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\
  \u05DD\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים (regex) ברובי הם דפוסים המשמשים להתאמת צירופי תווים במחרוזות, מה שמאפשר למפתחים לחפש, להתאים ולטפל בטקסט ביעילות. מתכנתים משתמשים בביטויים רגולריים למטרות כמו אימות, פרסור ומניפולציה של מחרוזות, מה שהופך אותם לכלי חיוני לעיבוד טקסט.

## איך ל:
### התאמה בסיסית
כדי להתאים מחרוזת לדפוס פשוט, ניתן להשתמש במתודת `match`. למטה, אנו בודקים אם המילה "Ruby" קיימת במחרוזת נתונה.

```ruby
if /Ruby/.match("Hello, Ruby!")
  puts "נמצאה התאמה!"
end
# Output: נמצאה התאמה!
```

### התאמת דפוסים עם משתנים
ניתן להטמיע משתנים בביטוי הרגולרי שלכם באמצעות תחביר ה-`#{}` , מה שהופך את הדפוסים שלכם לדינמיים.

```ruby
language = "Ruby"
if /#{language}/.match("תכנות בRuby זה כיף.")
  puts "מדברים על Ruby!"
end
# Output: מדברים על Ruby!
```

### שימוש בביטויים רגולריים לצורך החלפה
המתודה `gsub` מאפשרת לכם להחליף כל תופעה של דפוס במחרוזת החלפה שצוינה.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Output: barbarbar
```

### לכידה
סוגריים בביטוי רגולרי משמשים ללכידת חלקים מההתאמה. המתודה `match` מחזירה אובייקט מסוג `MatchData`, שניתן להשתמש בו כדי לגשת ללכידות.

```ruby
match_data = /(\w+): (\d+)/.match("Age: 30")
puts match_data[1] # תווית שנלכדה
puts match_data[2] # ערך שנלכד
# Output:
# Age
# 30
```

### שימוש בספריות צד שלישי
למרות שספריית הסטנדרט של רובי חזקה, לפעמים עשויים להיות צורך בפונקציונליות מתמחה יותר. גם עבור עבודה עם ביטויים רגולריים, `Oniguruma` הוא gem פופולרי, המספק תכונות נוספות מעבר למנוע הביטויים הרגולריים הטמע ברובי.

התקנה באמצעות:
```bash
gem install oniguruma
```

שימוש בדוגמה יכול להיראות כך (בהנחה שדרשתם את `oniguruma` לאחר ההתקנה):

```ruby
# זוהי דוגמה מתקדמת יותר ועשויה לדרוש הגדרה נוספת
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("מספר הוא 42.")
puts match_data[1]
# Output: 42
```

זכרו, למרות שהם חזקים, ביטויים רגולריים יכולים להפוך למורכבים וקשים לניהול עבור דפוסים מורכבים יותר. שאפו לקריאות, ושקלו שיטות חלופיות אם הביטוי הרגולרי שלכם הופך ליותר מידי מסובך.
