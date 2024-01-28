---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:19:41.245101-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML, המקוצר של Tom's Obvious, Minimal Language, הוא פורמט לארגון נתונים. מתכנתים אוהבים אותו בזכות פשטותו וקריאותו; הוא מעולה לקבצי תצורה, עם אווירה דומה ל-YAML אך פחות מסורבל מ-JSON עבור בני אדם.

## איך לעשות:
ראשית, התקן את `toml-cli` כדי לשחק עם TOML ב-Bash. שימושי לקרוא או לערוך קבצי TOML במהירות.

```Bash
# התקנת toml-cli, העוזר הקטן שלנו למשימות TOML
pip install toml-cli

# דמיין שיש לך קובץ TOML, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# קריאת ערך
toml get config.toml owner.name
# פלט: Tom

# הגדרת ערך
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# טיפ מקצועי: השתמש במרכאות עבור מפתחות עם נקודות או תווים מוזרים!
```

## צלילה עמוקה
נולד מהאי-נוחות ש-GitHub's חושש ממכשולים של JSON עבור בני אדם, TOML הופיע בערך בשנת 2013. טום פרסטון-וורנר, שותף מייסד של GitHub, רצה משהו קריא במיוחד. YAML ו- INI היו אלטרנטיבות אבל TOML כמו הטוב משניהם.

בדם קר, יש לך נתונים מקוננים ומערכים, פחות את תותחי הרגליים של YAML ואת הסוגריים המתולתלים של JSON. TOML הפך עכשיו לבחירה המועדפת לתצורה ב-Cargo של Rust, מה שמעיד על עלייתו בעולם הפיתוח. הוא מונע על ידי תקן, מה ששומר על דברים ברורים ומוגדרים היטב. תוכל לקבל ניתוחים בכמעט כל שפה, מה שהופך אותו לנפוץ ביותר.

## ראה גם
- מאגר GitHub הרשמי של TOML: https://github.com/toml-lang/toml
- toml-cli ב-PyPI: https://pypi.org/project/toml-cli/
- השוואה של פורמטים לארגון נתונים: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
