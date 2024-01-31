---
title:                "עבודה עם YAML"
date:                  2024-01-19
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט לשמירת נתונים בדמיון ל-JSON, אך ידידותי יותר לקריאה על-ידי בני אדם. מתכנתים עובדים איתו כי הוא פשוט לשימוש ונפוץ בתצורת יישומים ופרוייקטים.

## איך ל:
הנה דוגמה לקריאת קובץ YAML באמצעות Bash:

```Bash
# התקנת כלי yq (כמו jq, אבל ל-YAML)
sudo wget https://github.com/mikefarah/yq/releases/download/v4.6.3/yq_linux_amd64 -O /usr/bin/yq && sudo chmod +x /usr/bin/yq

# יצירת קובץ דוגמה example.yaml
cat <<EOT >> example.yaml
name: David
role: Developer
languages:
  - Hebrew
  - English
EOT

# קריאת שדה ספציפי (role) מתוך ה-YAML
yq e '.role' example.yaml
```

תצאה:
```
Developer
```

## פלונג פנימי
YAML (YAML Ain't Markup Language) נוצר ב-2001 כתחליף קל ונגיש ל-XML. על פני יש לו חלופות כמו JSON ו-TOML, אבל יתרונו בנוחות עבור משתמשים אנושיים. עבודה עם YAML ב-Bash בדרך כלל מצריכה כלים חיצוניים כמו yq מכיוון ש-Bash אינו תומך ב-YAML ישירות.

## ראה גם
- מדריך yq: https://mikefarah.gitbook.io/yq/
- מפרט YAML: https://yaml.org/spec/1.2/spec.html
- פוסט בלוג על ההבדלים בין YAML ל-JSON: https://phoenixnap.com/kb/yaml-vs-json
