---
title:                "עבודה עם ימל"
html_title:           "Fish Shell: עבודה עם ימל"
simple_title:         "עבודה עם ימל"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

# מה ולמה?
קודם כל, עבודה עם YAML היא דרך נוחה ופשוטה לארגון וניהול מידע מבני נתונים. תכנתנים משתמשים בזה כדי לנהל תצורות ותצורות עבור יישומים שונים בכדי לספק תצוגה נוחה ומובנת של המידע.

# איך לעבוד עם YAML בשל פרום פיש
כאשר אתם משתמשים בפרומ פיש, אתם יכולים לעבוד עם YAML בצורה פשוטה ונוחה. להלן כמה דוגמאות של קוד יישום המציגים כיצד ניתן להשתמש בזה:

```fish
set my_var (yaml get file.yaml my-key) 
# לקבלת ערך מנתוני YAML והמצביע על מפתח ספציפי בקובץ
```
```fish
for key in (yaml keys file.yaml)
echo $key
end
# להדפיס את כל המפתחות שנמצאים בקובץ YAML
```

# צלול אין עומק 
למרבה המזל, YAML נוצר כדי להיות תחליף קשיח של XML, שהיה מסובך וקושר לעבודה. אם אתה עדיין בספק כיצד לעבוד עם YAML או אם אתה מעדיף בחלוף כמה אלטרנטיבות, ישנם שני אפשרויות פופולריות אחרות - JSON ו- INI. גם אם YAML מבוסס על מפתחות וערכים כמו INI, יש לו גם את היכולת לייצג מבני נתונים מורכבים יותר כמו JSON. בנוסף, YAML מסוגל לנתח בין נתוני טבלה תאים ולפתח קבצים יותר מורכבים כפי שברוב קבצי התצורה.

# ראה גם
- [דף רשמי של פרומ פיש](https://fishshell.com)
- [תיעוד YAML הרשמי](https://yaml.org/)
- [קו מנחה עבור YAML בשל פרום פיש](https://fishshell.com/docs/current/commands.html#yaml)
- [דוגמאות של קבצי YAML עבור תחמולנים](https://segmentfault.com/a/1190000000651054)