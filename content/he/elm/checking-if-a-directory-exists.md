---
title:                "Elm: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##למה 

אנשים ירצו לבדוק אם תיקייה קיימת בשביל מספר סיבות. למשל, זה עשוי להיות חלק מאלגוריתם המחשב או שבמקרה שלנו, זה יכול להיות חלק מאפליקציית אלם.

##איך לעשות 

בשביל לבדוק אם תיקייה קיימת באפליקציית אלם, אנחנו משתמשים בפונקציית `Directory. exists` ומכניסים את הנתיב של התיקיה שאנחנו רוצים לבדוק. נחזור על התיקייה אם היא קיימת ונחזור `Nothing` אם היא לא קיימת. קדם כל דבר, נצטרך לייבא את המודול `Directory` בכדי להשתמש בפונקצייה.

```Elm
import Directory

directoryPath : String
directoryPath = "/Users/username/Documents/myDirectory"

checkDirectory : Maybe String
checkDirectory = Directory.exists directoryPath

-- שקרן את הפונקציה כאן ותקבל את התיקייה
-- או במקום זה תקבל נואשיות

```

##עיון מעמיק

כשאנחנו משתמשים בפונקציית `Directory.exists`, היא בעצם יוצרת את הכתובת המסלול לתיקייה ממאגר המידע של מכשיר המחשב. בכדי לוודא שהיא קיימת, היא מנסה לחפש את הכתובת במידע של הגדרות תיקיות המכשיר. אם הכתובת נמצאת, פונקציית `Directory.exists` תחזיר את התיקייה, או אחרת תחזיר `Nothing`.

##ראי איך

* [כיצד ליצור תיקייה חדשה באפליקציית אלם](https://guide.elm-lang.org/error_handling/handling_errors.html#creating-a-new-directory)
* [מסמכי אלם רשמיים](https://elm-lang.org/docs)
* [דוגמאות של אפליקציות אלם](https://github.com/jvoigtlaender/elm-projects)

##ראה גם

* [כיצד לבדוק אם קובץ קיים בשימוש באפליקציית אלם](https://github.com/elm-community/elm-file/blob/master/src/File.elm#L120)
* [השם של אותה פונקציית `Directory.exists` בשפת C](https://www.tutorialspoint.com/c_standard_library/c