---
title:                "C#: להצטרפות מחרוזות"
simple_title:         "להצטרפות מחרוזות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## על מה לכתוב

בואו נדבר על עקרונות בסיסיים בתכנות בשפת C#. אחד הדברים החשובים ביותר בכתיבת קוד הוא היכולת לשתף פעולה בין מספר תווים ליצירת מחרוזת אחת. במאמר זה על תמצית טיפול מחרוזת זו בשפת סי שארפ.

## איך לעשות זאת

לפני שנתחיל לכתוב קוד, נצטרך להגדיר מחרוזת נוספת. ניתן לעשות זאת בעזרת האופרטור + ולשתף פעולה בין שתי מחרוזות.

```C#
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + lastName;
Console.WriteLine(fullName);
```
Output: JohnDoe

כפי שאתם רואים, השתמשנו באופרטור + כדי לשתף פעולה בין שתי מחרוזות וליצור מחרוזת חדשה. אם ברצונכם להוסיף מרחב בין שני התווים, ניתן להוסיף גם מחרוזת ריקה כדי לייצר מרחב בין התווים.

```C#
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```
Output: John Doe

כעת, אם ברצונכם להוסיף ערכים אחרים, כגון מספרים או בוליאניים, יש להמיר אותם למחרוזות בעזרת הפונקציה .ToString כדי לשתף אותם פעולה בין שלושת המחרוזות.

```C#
int age = 30;
bool isMarried = false;
string fullName = firstName + " " + lastName + ". Age: " + age.ToString() + ". Married: " + isMarried.ToString();
Console.WriteLine(fullName);
```
Output: John Doe. Age: 30. Married: False

## מעמקים

כפי שראינו, הפעולה של שתף פעולה בין מחרוזות בשפת C# היא פעולה פשוטה ויעילה. במיוחד כאשר יש לנו צורך ליצור מחרוזת מסובכת עם מספר ערכים שונים. חשוב לזכור שעלינו להמיר ערכים שלא מסוג מחרוזת למחרוזת בעזרת הפונקציה .ToString כדי לשתף אותם פעולה בין מחר