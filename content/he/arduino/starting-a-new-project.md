---
title:    "Arduino: התחלת פרויקט חדש"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# בכל יילדיכם אחים-לומְד

 שלום לכם קוראיי היקרים! היום נדבר על מחשב קטן ומדהים בשם ארדואינו. אם אתם מתעניינים בתחום הטכנולוגיה ורוצים לפתח פרויקט חדש, המאמר הזה בשבילכם! ארדואינו הוא מיני-מחשב הנועד להרכבה של פרויקטים אלקטרוניים פשוטים ומתקדמים. מי שמתעניין בתחום התכנות והאלקטרוניקה, יכול להשתמש בארדואינו ליצירת פרויקטים משלו.

## मज़ा क्यों उठाना होगा

בכל פרויקט חדש שנתחיל, יש הרגשה נפלאה של עבודה בקבוצה עם משנת ופתרון בעיות יחד. אם אתם אוהבים לפתור בעיות וליצור דברים חדשים, ארדואינו הוא בדיוק עבורכם. בנוסף, מעבר להנאה והאתגר, זה יכול להיות תחום משמעותי ביותר בעתיד עם יתרונות רבים לספקי העבודה.

## איך לעשות

לפני שנתחיל לתכנת בארדואינו, נצטרך להוריד ולהתקין את התוכנה המתאימה. לאחר מכן, אנו נצטרך לחבר את ארדואינו למחשב עם כבל USB. לאחר מכן, נוכל לפתוח את התוכנה ולהתחיל לתכנת את הקוד לפרויקט שלנו. הנה דוגמא פשוטה של קוד ותוצאה לפרויקט שמדהים:

```Arduino
int LED = 13; // פינה של הפלט הנורית

void setup()
{
  pinMode(LED, OUTPUT); // הגדרת הפלט כפלט נורית
}

void loop()
{
  digitalWrite(LED, HIGH); // הדלקת הנורית
  delay(1000); // המתנה של שנייה
  digitalWrite(LED, LOW); // כיבוי הנורית
  delay(1000); // המתנה של שנייה
}
```

תוצאה:

![נורית דולקת