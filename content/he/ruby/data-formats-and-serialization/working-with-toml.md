---
date: 2024-01-26 04:26:19.067014-07:00
description: "TOML \u05D4\u05D9\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E7\u05D5\u05D1\
  \u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05E9\u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\
  \u05D0 \u05D1\u05E9\u05DC \u05D4\u05E1\u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\
  \u05D1\u05E8\u05D5\u05E8\u05D4 \u05E9\u05DC\u05D5. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-TOML \u05DB\u05D3\u05D9\
  \ \u05DC\u05E0\u05D4\u05DC \u05EA\u05E6\u05D5\u05E8\u05D5\u05EA \u05D0\u05E4\u05DC\
  \u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05D5\u05E1\u05E8\u05D9\u05D0\u05DC\u05D9\u05D6\
  \u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05DC\
  \u05D0 \u05D4\u05E0\u05D8\u05DC \u05E9\u05DC XML \u05D0\u05D5\u2026"
lastmod: 2024-02-19 22:04:59.523791
model: gpt-4-0125-preview
summary: "TOML \u05D4\u05D9\u05D0 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E7\u05D5\u05D1\
  \u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05E9\u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\
  \u05D0 \u05D1\u05E9\u05DC \u05D4\u05E1\u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\
  \u05D1\u05E8\u05D5\u05E8\u05D4 \u05E9\u05DC\u05D5. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-TOML \u05DB\u05D3\u05D9\
  \ \u05DC\u05E0\u05D4\u05DC \u05EA\u05E6\u05D5\u05E8\u05D5\u05EA \u05D0\u05E4\u05DC\
  \u05D9\u05E7\u05E6\u05D9\u05D5\u05EA \u05D5\u05E1\u05E8\u05D9\u05D0\u05DC\u05D9\u05D6\
  \u05E6\u05D9\u05D4 \u05E9\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05DC\
  \u05D0 \u05D4\u05E0\u05D8\u05DC \u05E9\u05DC XML \u05D0\u05D5\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
---

{{< edit_this_page >}}

## מה ולמה?

TOML היא פורמט קובץ תצורה שקל לקרוא בשל הסמנטיקה הברורה שלו. תכנתים משתמשים ב-TOML כדי לנהל תצורות אפליקציות וסריאליזציה של נתונים ללא הנטל של XML או המשונויות של YAML.

## איך לעשות:

ראשית, התקן את הגימ 'toml-rb'. זהו בחירה פופולרית לניתוח TOML ב-Ruby.

```Ruby
gem install toml-rb
```

לאחר מכן, קריאה מקובץ TOML:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

דוגמה לפלט עשויה להיות:

```
My Awesome App
```

כתיבה לקובץ TOML:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

בדוק את `config.toml` ותראה את ההגדרות שלך, מאוחסנות בסדר.

## צלילה עמוקה

TOML, שמציין "Tom's Obvious, Minimal Language", נוצר על ידי טום פרסטון-וורנר, שותף-מייסד של GitHub, בערך ב-2013. המטרה הראשית שלו היא להיות פורמט פשוט שקל לפרסר אל מבני נתונים. בעוד JSON מצוין ל-APIs, ו-YAML גמיש, המקום המיוחד של TOML הוא הדגש על ידידותיות לאדם. בניגוד ל-YAML, שיכול להיות קפדני לגבי הזחה, TOML שואף למבנה דומה יותר ל-INI שרבים מוצאים פשוט יותר ופחות נתון לשגיאות.

אלטרנטיבות כמו JSON, YAML, או XML כל אחד עם היתרונות שלו, אך TOML מצליח בתסריטים שבהם קובץ תצורה צריך להיות קל לתחזוקה על ידי בני אדם ותוכניות כאחד. זה לא רק פשוט יותר אלא מכתיב עיצוב קריא ומדויק.

מהצד הטכני, כדי לפרסר תוכן TOML עם Ruby, אנו מנצלים גימים כמו `toml-rb`. גים זה מנצל את האופי הדינמי של Ruby, ממיר נתוני TOML למערכים, להאשים ולמבני נתונים בסיסיים אחרים ב-Ruby. המרה זו אומרת שמפתחים יכולים לעבוד עם נתוני TOML באמצעות הסמנטיקה והמתודות המוכרות של Ruby.

## ראה גם

- פרויקט ומפרט TOML: https://toml.io/en/
- הגים `toml-rb`: https://github.com/emancu/toml-rb
- השוואת TOML, YAML, ו-JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
