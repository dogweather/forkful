---
title:                "שימוש במנפה שגיאות"
date:                  2024-01-26T03:49:57.998113-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במנפה שגיאות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?
השימוש בדיבאגר הוא בעצם אתה מתחקר בקוד שלך, מחפש תקלות ומנסה להבין למה הדברים לא פועלים חלק. תכנתנים עושים את זה כי, בואו נתמודד עם זה, תקלות הן בלתי נמנעות, ודיכוי שלהן ביעילות אומר לקבל את הקוד שלך פועל מהר יותר ובאופן אמין יותר.

## איך לעשות:
כיום, Gleam נשען על האקוסיסטם של Erlang בכל הנוגע לכלים, אז בדרך כלל תתכנן בעזרת כלים כמו `rebar3`, `observer`, ו-`debugger`. הנה איך להתחיל ולהתלכלך עם דיבוג:

```gleam
// בהגדרות rebar שלך, ודאו שיש לכם את השורות האלה לכלול מידע דיבוג:
{erl_opts, [debug_info]}.

// הפעל של Erlang עם האפליקציה שלך טעונה
rebar3 shell

// בתוך השל, אתה יכול להתחיל את הדיבאגר
1> debugger:start().
```

פשוט, נכון? חלון ה-`debugger` GUI קופץ, ואתה יכול להגדיר נקודות עצירה, לעבור דרך הקוד, ולצפות במשתנים ככל שתרצה. לא תראה קוד של Gleam באופן ישיר, אלא את הקוד של Erlang שלו הוא מתורגם, שזה עדיין די מועיל.

## צלילה אל העומק
Gleam הוא שפה צעירה, אז בעוד שהיא עומדת על כתפיים של האקוסיסטם של Erlang, כלים ייחודיים לדיבוג של Gleam עדיין לא בחזית. זה אומר שאנחנו משתמשים בכלים הנוסים והנכונים של Erlang, וזו לא דבר רע. הדיבאגר של Erlang קיים מאז שנות ה-90, מושפל במשך שנים של סילוק חרקים מעקשים במערכות שבהן האמינות היא המפתח.

לחלופין, איתור היא שיטה חזקה בעולם של BEAM (זו המכונה הווירטואלית שמריצה קוד של Erlang ו-Elixir). באמצעות `rebar3` אתה יכול להיכנס לכלים כמו `recon` כדי לעקוב אחרי קריאות פונקציות ולחקור עמוק יותר בעיות ביצועים.

המעבר בין כתיבה ב-Gleam לדיבוג ב-Erlang עלול להרגיש כאילו אתה מתרגם את המחשבות שלך בזמן אמת. אבל היתרון הוא שאתה מקבל הצצה לעולם של Erlang, מבין את בסיסי היישום שלך בצורתו הרצה.

## ראה גם
כדי להרחיב את ערכת הכלים שלך לדיבוג, בדוק:

- תיעוד הדיבאגר של Erlang: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- הספריה `recon` עבור Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- על איתור ב-BEAM: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)