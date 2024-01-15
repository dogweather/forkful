---
title:                "עבודה עם Yaml"
html_title:           "Fish Shell: עבודה עם Yaml"
simple_title:         "עבודה עם Yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

לאחרונה, YAML הפך לשפת תצורה נפוצה ומועילה בקוד. מתוך מודעות זאת, השתמש בפיש של קוד בכדי לפענח נתוני YAML היא תוכנית נפוצה ויעילה יותר בכדי לעזור לך להתאים את הקובץ לצרכיך המיוחדים.

## איך לעשות זאת

כאשר מדברים על פיש, עלינו להתחיל עם נסיונות מסוימים שיסייעו לנו לדאוג שהקוד שלנו יתאים לקובץ YAML שברצוננו לפענח אותו. כאן כמה דוגמאות שימושיות לעבודה עם YAML בפיש:

```Fish Shell
set yaml (cat file.yaml) # כאן אנחנו משתמשים בפקודת הדפסה בכדי לראות את תוכן הקובץ. המידע הוא מצוי במשתנה "yaml".
echo $yaml # כאן אנחנו מדפיסים את התוכן שנמצא במשתנה "yaml".
yq r file.yaml # פקודת פיתוח נתונים פשוטים המאפשרת לנו לעבוד ישירות עם קובץ YAML.
```

כאשר אנו מבינים את הבסיסים של העבודה עם YAML בפיש, ניתן לפתח את הכישורים שלנו כדי להתמודד עם נתונים מורכבים יותר ובסוף להצליח בכל פרויקט שמאמין עם נתוני YAML.

## כיון מעמיק

ייתכן שתרצה להסתכל על משאבים נוספים בתחום העבודה עם YAML בפיש. אתה יכול למצוא מדריכים מפורטים יותר והכישורים שלך יתעצמו עם כל חומר הדגל. אתה יכול גם להתנסות עם כלים נוספים כמו jq כדי לפענח נתונים מורכבים יותר בפורמט YAML.

## ראו גם

- [מדריך לפיש חומר בנייה הקוד של YAML](https://fishshell.com/docs/current/tutorial.html#building-your-code-with-yaml-materials)
- [תיעוד ר