---
title:                "מחיקת תווים התואמים דפוס"
html_title:           "Go: מחיקת תווים התואמים דפוס"
simple_title:         "מחיקת תווים התואמים דפוס"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

מחיקת תווים המתאימים לתבנית היא כלי חשוב בתכנות בשפת גו, המאפשר למתכנתים לנתח ולערוך מחרוזות בצורה יעילה ומדויקת.

## איך לעשות זאת

```Go
func DeleteChars(str, pattern string) string {
    output := strings.ReplaceAll(str, pattern, "")
    return output
}

func main() {
    str := "Hello World!"
    newStr := DeleteChars(str, "l")
    fmt.Println(newStr) // Prints "Heo Word!"
}
```

הפונקציה "DeleteChars" מקבלת שני פרמטרים: מחרוזת ותבנית למחיקה. בעזרת פונקציית "ReplaceAll" מספרי המחרוזת מחולפים לפי התבנית והתוצאה מוחזרת. בעיה פרושה על ידי הפונקציה "main", וברזולטט תקבלו את המחרוזת המותאמת עם התווים שנמחקו.

## בירור עמוק

פונקציית "ReplaceAll" נמצאת בחבית "strings" והיא מאפשרת למתכנתים לבצע שינויים במחרוזות לפי תבנית מסוימת. הפונקציה מחזירה את אותה מחרוזת עם התווים המתאימים לתבנית מחולפים לפי התו המוגדר. כך ניתן לבצע מחיקת תווים במחרוזות פשוט ומהיר בעזרת שפת גו.

## ראו גם

- [ReplaceAll documentation](https://golang.org/pkg/strings/#ReplaceAll)
- [Go Tutorial - Working with Strings](https://www.tutorialspoint.com/go/go_working_with_strings.htm)
- [Deleting Characters in Strings with Go](https://www.dotnetperls.com/tr
ials/go/strings-replace)