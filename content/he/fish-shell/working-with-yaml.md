---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט שמשמש לייצוג נתונים בצורה נגישה לקריאה על ידי בני אדם. תכניתנים משתמשים בו להגדרת קונפיגורציות, מבנים ותיעוד כי הוא פשוט, גמיש ונפוץ.

## איך לעשות:
כדי לעבוד עם קבצי YAML ב-Fish, ניתן להשתמש בפקודות ואבזרים חיצוניים כמו `yq`. זה דוגמא לתסריט פשוט:

```Fish Shell
# התקן את yq
sudo apt-get install yq

# פרס קובץ YAML והצג את הערך מתחת למפתח 'user'
yq e '.user' config.yaml
```

נניח שיש לנו `config.yaml` עם התוכן הבא:
```yaml
user:
  name: "dvora"
  role: "developer"
```

הפלט יהיה:
```
name: "dvora"
role: "developer"
```

## עיון עמוק
YAML (YAML Ain't Markup Language) הוא קרוי "אנטי מארקאפ" בשעשוע. הוא נוצר ב-2001 לסייע במשימות תכנות בהן XML היה כבד ומסובך. תחליפים כוללים JSON ו-TOML. כאשר עובדים עם YAML ב-Fish, זכור להסתמך על כלים חיצוניים כי Fish אינו מספק תמיכה ישירה ב-YAML כמו שפות אחרות.

## ר' גם
- [YAML ויקיפדיה](https://he.wikipedia.org/wiki/YAML)
- [מדריך ל-Fish Shell](https://fishshell.com/docs/current/index.html)
- [עמוד הגיטהאב של yq](https://github.com/mikefarah/yq)
