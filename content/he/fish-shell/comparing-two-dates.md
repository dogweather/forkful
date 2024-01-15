---
title:                "השוואת שתי תאריכים"
html_title:           "Fish Shell: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

למה אדם יהיה מעוניין להשים שתי תאריכים? כי זה יכול להיות כלי שימושי בדיבורים ובתכניתת היספורטציה.

## איך לעשות

תהליך השוואת שתי תאריכים בפיש של (גירסה נוכחית) נעשה באמצעות פונקציית `date` כדי להחזיר את תאריך המעבר בפורמט הרצוי. ניתן להשתמש בערכים של התאריך הרצויים (לדוגמה, ימים, חודשים, שנים) על מנת לבצע את הפעולות הנחוצות. להלן דוגמאות לשימוש של פקודת הפיש ביחס להשוואת שני תאריכים:

```
Fish Shell> date -d "10/5/2021" +%Y-%m-%d
2021-10-05
```

```
Fish Shell> set startDate (date -d "10/5/2021" +%s)
Fish Shell> set endDate (date -d "10/10/2021" +%s)
Fish Shell> math "($endDate - $startDate)/86400" # כמות הימים בין התאריכים
5
```

## מעמקי

עבור מי שמעוניין במידע נוסף על השוואת שתי תאריכים, ניתן לשלב פקודת הפיש עם פקודת `grep` כדי לחפש מילים מסוימות בפלט. לדוגמה, אם נרצה לבדוק אם יש הבדל בין שני תאריכים בחודש ינואר, נוכל להשתמש בפקודה הבאה:

```
Fish Shell> set startDate (date -d "1/10/2021" +%s)
Fish Shell> set endDate (date -d "1/20/2021" +%s)
Fish Shell> math "($endDate - $startDate)/86400" | grep "30" # בדיקת ההבדל בימים
```

בנוסף, ניתן להשתמש בפקודת הפיש `date` כדי להמיר את התאריך לפורמט נוח יותר, כך שהשוואה בין שני תאריכים תהיה פשוטה יותר.

## ראו גם

* [תיעוד רשמי של פקודת הפיש לחישוב תאריכים](https://fishshell.com/docs/current/cmds/date.html)
* [אתר המדריך הרשמי של פיש](https://fishshell.com/docs/current/index.html)