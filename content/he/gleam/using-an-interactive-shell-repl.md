---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:15:00.013043-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## מה ולמה?

REPL, המקוצר של Read-Eval-Print Loop (לולאת קריאה-הערכה-הדפסה), היא כלי תכנות המאפשר ריצה אינטרקטיבית של קוד וראיית תוצאות מיידית. מתכנתים משתמשים בה לניסוי, ניפוי באגים, או למידת שפה חדשה על המהלך כמו Gleam.

## איך לעשות:

כרגע, Gleam אינה כוללת REPL בתפוצה הסטנדרטית שלה. עם זאת, ניתן לנסות קוד Gleam באמצעות המעטפת של Erlang, מכיוון ש-Gleam מתורגמת לבייטקוד של Erlang. כך תעשו זאת:

1. תרגמו את קוד ה-Gleam שלכם ל-Erlang.
```plaintext
gleam build
```

2. הפעילו את מעטפת ה-Erlang.
```plaintext
erl -pa ebin
```

3. קראו לפונקציות ה-Gleam שלכם (בהנחה שיש לכם מודול בשם `my_mod` ופונקציה `my_fun`).
```erlang
my_mod:my_fun().
```

אתם אמורים לראות את פלט הפונקציה מוצג במעטפת.

## צלילה עמוקה

REPL מגלם את הרוח הדינמית והחקרנית של שפות תכנות פונקציונליות רבות, ומסלד אחרי רפל של LISP בשנות ה-60. בהשוואה, מערכות אחרות כמו `ipython` של Python או `irb` של Ruby מציעות חוויות דומות לקהילות שלהן.

אף על פי ש-Gleam עדיין אינה מכילה REPL ייחודית, השימוש במעטפת ה-Erlang נותר פתרון מהיר ונוח. היכולות של מעטפת ה-Erlang מגיעות מ-VM של BEAM, מכונת הווירטואלית שמעניקה כוח לאקוסיסטם של Erlang, שכולל את Elixir, LFE, ו-Gleam.

חלופות ל-REPLs באקוסיסטם של Gleam עשויות לכלול כתיבת מקרי בדיקה או שימוש במהדרים ומגרשי משחקים אונליין התומכים ב-Gleam, לבדיקת קטעי קוד מחוץ להגדרת פרויקט מלאה.

היישום של REPL ייחודית ל-Gleam נתקל באתגרים בעיקר בגלל האופי המקומפל של Gleam וזמן הריצה של Erlang, שבו החלפת קוד חמה היא הנורמה. כל REPL עתידית של Gleam תצטרך להתגבר על סוגת הטיפוס הסטטי של השפה עם סביבת הביצוע הדינמית ש-REPL מצפה.

## ראו גם

- התיעוד הרשמי של Gleam: https://gleam.run/book/
- תיעוד מעטפת ה-Erlang: http://erlang.org/doc/man/erl.html
- מגרש משחקים להרצת קוד Gleam באינטרנט: https://gleam.run/compiler/