---
title:                "Python: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/working-with-yaml.md"
---

{{< edit_this_page >}}

##למה
אנשים מתעסקים עם YAML בשביל שפת התכנות היא מאוד תואמת לשפה טבעית ומאפשרת כתיבה וקריאה של כמה איביים בפורמט מסודר.

##כיצד לעשות זאת
```python
import yaml

# יצירת קובץ YAML חדש
data = {'משתמש': 'גל', 'גיל': 29, 'עיר': 'תל אביב'}
with open('user.yaml', 'w') as file:
    yaml.dump(data, file)

# קריאת קובץ YAML קיים
with open('user.yaml', 'r') as file:
    user = yaml.load(file)

print(user)
# Output: {'משתמש': 'גל', 'גיל': 29, 'עיר': 'תל אביב'}
```

##חקירה מקיפה
הפורמט YAML נוצר בפועל על ידי מתכנתים טבעים שהבחינו כי לשפת הפיתוח הקיימת חסרון משמעותי בכתיבה וקריאה של תצורה וגיבוב של נתונים. השפה מותאמת כשפה אנוכנית שמתלבשת בכיסוי הלוחם של השפה פייתון. בעבודה עם YAML ניתן לטעון כמה איביים מקובלים בפורמט המוכר כ JSON ובתוספת זאת ניתן לכתוב איביים שמתארים את נתוני התצורה של תוכנית.

כמו גם, ישנם כלים מגוונים לעבודה עם פורמט YAML כגון ספריית PyYAML שמאפשרת ייבוא ויצוא של נתונים מפורמט זה.

##ראה גם
- [מסמך המפרט את פורמט ה-YAML](https://yaml.org/spec/1.2/spec.html)
- [ספריית PyYAML](https://github.com/yaml/pyyaml)
- [מדריך לשפת פייתון](https://www.python.org/)