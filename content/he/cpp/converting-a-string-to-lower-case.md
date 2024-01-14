---
title:                "C++: כתיבת מחרוזת לצורת האותיות הקטנות"
simple_title:         "כתיבת מחרוזת לצורת האותיות הקטנות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##למה:
להמיר מחרוזת לאותיות קטנות יכול להיות שימושי כאשר מתעסקים עם טקסטים משתמשים, וזה יכול לסייע בבדיקת משתנים או לטפל בעבור מקורות טסטים שונים.

##איך לעשות זאת:
הקוד הבא מציג דרך פשוטה להמיר מחרוזת לאותיות קטנות באמצעות פרמטר ומחזיר את התוצאה בעזרת הפונקציה tolower( ) שלשפת סי-פלוספלוספלוספלוס:

```C++
std::string convertToLowercase(std::string str) 
{ 
    int n = str.length(); 
    for (int i = 0; i < n; i++) { 
        str[i] = tolower(str[i]); 
    } 
    return str; 
} 

int main() 
{ 
    std::string str = "C++ Programming"; 
    std::cout << "Original string: " << str << std::endl; 
    str = convertToLowercase(str); 
    std::cout << "Converted to lowercase: " << str << std::endl; 
    return 0; 
} 
```

###פלט:
```
Original string: C++ Programming
Converted to lowercase: c++ programming
```

##מידע עמוק:
המרת מחרוזת לאותיות קטנות היא תהליך פשוט שקיים ברוב לשונות התכנות ומשמש פעולת בסיס עבור טיפול בטקסט. בכדי להמיר את המחרוזת לעובדת, אנו משתמשים בפונקציה tolower( ) שמקבלת כפרמטר את התו או התווים הנבחרים ומחזירה את הערך המתאים של האותיות קטנות באמצעות אסקיודים.

##ראה גם:
- תיעוד על פונקציית tolower( )של C++: https://www.cplusplus.com/reference/cctype/tolower/
- עוד ניסיונות להמרת מחרוזת לאותיות קטנות בשפת סי-פלוספלוספלוספלוס: https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/