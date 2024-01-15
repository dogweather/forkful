---
title:                "עובדים עם קבצי CSV"
html_title:           "C#: עובדים עם קבצי CSV"
simple_title:         "עובדים עם קבצי CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

"## למה" "הדרך לעשות את זה"
"## שופט הגבולות"

### למה

ישנם מספר סיבות נהדרות לעבוד עם CSV בכתיבת קוד C#. בין היתר, זה מאפשר לנו להעביר נתונים בפורמט אשר ניתן לקריאה ונוח לעריכה על ידי משתמשים, תוכניות ומערכות אחרות. כמו כן, זה יכול להיות רב תכליתי ולשמש למגוון שימושים, כגון יציאה לקובץ או יבוא נתונים ממקורות שונים.

## הדרך לעשות את זה 

ניתן לעבוד עם CSV בכמה שיטות שונות באמצעות קוד C#. לדוגמה, נוכל להשתמש במחלקות תומכות הנמצאות בתוך הספריה המובנית של .NET, כמו `File` ו-`Stream`. ניתן גם להוריד חבילות חיצוניות כמו "CSV Helper" או "CsvReader" ולהשתמש בהם לטיפול בנתונים בפורמט CSV.

במקרה שאנחנו רוצים ליצור קובץ CSV, נוכל להשתמש במחלקות `StreamWriter` ו- `StringBuilder` כדי ליצור טקסט בפורמט המתאים. ניתן להשתמש בלולאות ותנאים כדי לעצב את הנתונים לפי הצורך. לדוגמה:

```C#
// יצירת קובץ CSV חדש
using (StreamWriter writer = new StreamWriter("new_file.csv"))
{
    StringBuilder sb = new StringBuilder();
    // הוספת כותרת העמודות לקובץ
    sb.AppendLine("שם,תאריך לידה,מין");
    // יצירת נתונים עבור שני משתמשים
    string[] user1 = { "דוד", "15/05/1987", "גבר" };
    string[] user2 = { "אנה", "03/09/1991", "נקבה" };
    // הוספת נתוני המשתמשים לתוך הקובץ
    for (int i = 0; i < user1.Length; i++)
    {
        sb.Append(user1[i]);
        sb.Append(",");
        sb.Append(user2[i]);
        sb.Append(",");
    }
    // כתיבת הנתונים לקובץ
    writer.WriteLine(sb.ToString());
}
```

במקרה שאנחנו רוצים לטפל בנתונים שמ