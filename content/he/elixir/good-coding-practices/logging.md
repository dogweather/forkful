---
date: 2024-01-26 01:03:47.847628-07:00
description: "Logging \u05D1\u05E4\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D5\u05DB\u05E0\
  \u05D4 \u05D4\u05D5\u05D0 \u05D4\u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05E9\u05DC\
  \ \u05E8\u05D9\u05E9\u05D5\u05DD \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD \u05E9\
  \u05E7\u05D5\u05E8\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF \u05E9\u05D4\u05EA\u05D5\
  \u05DB\u05E0\u05D4 \u05E4\u05D5\u05E2\u05DC\u05EA, \u05D1\u05D3\u05E8\u05DA \u05DB\
  \u05DC\u05DC \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D0\u05D5 \u05DE\u05E2\u05E8\u05DB\
  \u05EA \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05E7\u05D1\u05DC \u05D4\u05D1\u05E0\u05D4 \u05D8\u05D5\u05D1\u05D4 \u05D9\u05D5\
  \u05EA\u05E8 \u05E9\u05DC\u2026"
lastmod: '2024-03-13T22:44:38.784435-06:00'
model: gpt-4-1106-preview
summary: "Logging \u05D1\u05E4\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D5\u05DB\u05E0\u05D4\
  \ \u05D4\u05D5\u05D0 \u05D4\u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05E9\u05DC \u05E8\
  \u05D9\u05E9\u05D5\u05DD \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD \u05E9\u05E7\
  \u05D5\u05E8\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF \u05E9\u05D4\u05EA\u05D5\u05DB\
  \u05E0\u05D4 \u05E4\u05D5\u05E2\u05DC\u05EA, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\
  \u05DC \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D0\u05D5 \u05DE\u05E2\u05E8\u05DB\u05EA\
  \ \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E7\
  \u05D1\u05DC \u05D4\u05D1\u05E0\u05D4 \u05D8\u05D5\u05D1\u05D4 \u05D9\u05D5\u05EA\
  \u05E8 \u05E9\u05DC\u2026"
title: "\u05DC\u05D5\u05D2\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
Logging בפיתוח תוכנה הוא הטכניקה של רישום אירועים שקורים בזמן שהתוכנה פועלת, בדרך כלל לקובץ או מערכת חיצונית. תכנתים עושים זאת כדי לקבל הבנה טובה יותר של התנהגות התוכנה, לטפל בבעיות, ולשמור רישום של ההיסטוריה התפעולית שהוא חיוני לאיתור באגים וניטור הבריאות של אפליקציות.

## איך לעשות זאת:
ב-Elixir, הדרך העיקרית לרשום מידע היא דרך מודול ה-`Logger` המובנה. כך תוכלו להשתמש בו:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("מתחיל תהליך חשוב עם פרמטר: #{param}")

    # סימולציה של עבודה שנעשית
    :timer.sleep(1000)

    Logger.debug("התהליך הושלם.")
  rescue
    error -> Logger.error("אירעה שגיאה: #{inspect(error)}")
  end
end

# כדי לראות את הלוגים שלך, פשוט קרא לפונקציה:
MyApplication.do_something_important("MyParam")
```

קטע קוד זה מראה איך לרשום ברמות שונות (`info`, `debug`, ו-`error`). כאשר אתם מריצים את זה, לא תראו את ההודעה `debug` אלא אם תקנפגו את רמת ה-`Logger` ל-`:debug`. כברירת מחדל, `Logger` של Elixir מסנן הודעות לוג מתחת ל-`:info`.

דוגמת פלט ברמת `:info` עשויה להיראות כך:
```
14:32:40.123 [info]  מתחיל תהליך חשוב עם פרמטר: MyParam
14:32:41.126 [error] אירעה שגיאה: %RuntimeError{message: "שגיאת ריצה"}
```

## נסיון עמוק יותר:
`Logger` ב-Elixir הוא כלי מובנה שהיה חלק מהשפה מתחילת דרכה. הוא מושפע מהמערכות הלוגים של שפות BEAM אחרות כמו Erlang. הלוגר מספק רמות לוגים שונות – `:debug`, `:info`, `:warn`, ו-`:error` – והוא ניתן להתקנה, מאפשר לחבר backends שונים לטיפול בהודעות הלוג.

אלטרנטיבה ל-`Logger` המובנה לסצנריות מורכבות יותר היא שימוש בספריות לוגים כגון `Logstash` או `Sentry` ל-Elixir, שיכולות לספק יכולות נוספות כמו מעקב אחרי שגיאות ואגרוגציה בפורמט יותר ויזואלי. לפיתוח מקומי, מפתחי Elixir לעיתים נשענים על הפונקציונליות המובנית של Logger בשל פשטותה ואינטגרציה עם מכונת ה-BEAM VM.

מאחורי הקלעים, מודול ה-Logger מציע לוגים אסינכרוניים וסינכרוניים. לוגים אסינכרוניים, שהם ברירת המחדל, אינם חוסמים את ביצועי האפליקציה בעת רישום ההודעות. זה מבטיח שהלוגינג אינו משפיע לרעה על הביצועים. עם זאת, ניתן להפעיל לוגים סינכרוניים למקרים בהם אתם צריכים להבטיח שההודעות נרשמות בסדר שבו נשלחו.

ניתן להתאים את תצורת Logger בקובץ `config/config.exs` של אפליקציה ב-Elixir, שם תוכלו לקבוע את רמת הלוגים, הפורמט, מטא־דאטה, ועוד. תמיד זכרו לכוון מחדש את רמת הלוגים והפלטים לסביבות שונות; אינכם רוצים שלוגים רבי פרטניות של דיבאג יציפו את מערכות הייצור שלכם.

## ראה גם:
- התיעוד הרשמי של Logger ב-Elixir: https://hexdocs.pm/logger/Logger.html
- פוסט בבלוג על המעשה הטובים ביותר של לוגים ב-Elixir: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry ל-Elixir ב-Hex: https://hex.pm/packages/sentry
- שיעור של בית הספר של Elixir על Logger: https://elixirschool.com/en/lessons/specifics/debugging/#logging
