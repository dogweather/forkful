---
title:                "התחלת פרויקט חדש"
html_title:           "C#: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

##למה
כשמתחילים פרויקט חדש, הם יכולים ליצור משהו חדש ויצירתי. זה מספק גירוי ואתגר, ויכול לעודד תחושת ההשגה האישית.

##איך לעשות זאת
התחברו לסביבת עבודת הפתוח שלכם ונסו את הדוגמאות הבאות בתוך קוד C#. תוכלו לראות את התוצאות בדוגמאות הקוד המצורפות. 

```C#
// צור פרויקט חדש
Console.WriteLine("שלום עולם!");
```
Output: שלום עולם!

```C#
// הוסף קוד ובצע הדפסה למסך
Console.Write("שלום ");
Console.WriteLine("עולם!");
```
Output: שלום עולם!

```C#
// קבל קלט מהמשתמש והדפס אותו למסך
Console.WriteLine("בבקשה, הכנס את שמך:");
string name = Console.ReadLine();
Console.WriteLine("שלום " + name + "!");
```
Output:
בבקשה, הכנס את שמך:
(משתמש מזין את השם שלו)
שלום (שם שהמשתמש מזין)!

##תהליך מקיף
כדי להתחיל פרויקט חדש ב-C#, ישנם כמה צעדים חשובים שצריך לנקוט:

1. להתקין את תוכנת הפיתוח של C# (בדרך כלל חלק מהתכנים הנמצאים בתוך סטודיו ויזואל)
2. ליצור פרויקט חדש
3. ליצור קבצי קוד ולהוסיף להם תוכן
4. לבנות ולהריץ את הפרויקט
5. לשלוח את הקוד לשיתוף עם אנשים אחרים ולקבל משוב עליו
6. להמשיך לשפר ולהשתפר בכל פעם שמתחילים פרויקט חדש.

##ראו גם
- [מדריך ליצירת פרויקט ראשון ב-C#](https://docs.microsoft.com/he-il/dotnet/csharp/tutorials/intro-to-csharp/start-project)
- [ספריית התיעוד הרשמית של C#](https://docs.microsoft.com/he-il/dotnet/csharp/)