---
title:                "Fish Shell: עבודה עם csv"
simple_title:         "עבודה עם csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

# למה

CSV הוא פורמט מקובל לאחסון והעברת נתונים טבלאיים. במאמר זה נלמד כיצד לעבוד עם קבצי CSV באמצעות פקודות של שפת התסריטים "Fish Shell". המאמר יכיל קוד מפורט ודוגמאות כדי לסייע לקוראים להתחיל בעבודה עם CSV בצורה נוחה ויעילה.

# איך לעשות זאת

קוד הדגמה יעזור בהכרת סינטקסיס של פקודות של "Fish Shell" לפענוח, כתיבה ועריכת קבצי CSV. כדי להתחיל, נדפיס את כל השורות בקובץ CSV באמצעות הפקודה "cat" ונקטוע את המידע לכן.

```Fish Shell
cat data.csv
tail -n +2 data.csv
```

לכל שורה בקובץ, נדפיס את התא הראשון עם הפקודה "cut" ואת הערך השני עם הפקודה "awk". בסיום, נשמור את תוצאת הפלט בקובץ חדש באמצעות הפקודה "tee".

```Fish Shell
cut -d, -f1 data.csv | awk '{print $2}' 
tee output.csv
```

נוכל גם להשתמש בפקודת "grep" כדי למצוא מידע מסוים בקובץ CSV ולהציג את התוצאה באמצעות הפקודה "echo".

```Fish Shell
grep "John" data.csv | echo
```

כעת נבין כיצד לעשות עבודות פסק זמן קצרות יותר עם קבצי CSV באמצעות פקודות של "Fish Shell".

# הכי עמוק

שפת התסריטים "Fish Shell" מכילה גם כלים מתקדמים כדי לעבוד עם קבצי CSV. לדוגמה, נוכל להשתמש בפקודת "sed" כדי לערוך את הקובץ CSV במקום ליצור קובץ חדש. ניתן להעתיק עמודות באמצעות הפקודה "cp" ולהוסיף נתונים חדשים עם הפקודה "echo".

```Fish Shell
sed -i 's/John/David/g' data.csv
cp data.csv data_copy.csv
echo "Tom, 45, Boston" >> data.csv
```

ב