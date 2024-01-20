---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

קריאת ארגומנטים מקו הפקודה ב- C++ היא מדובר ביכולת לקבל קלט מהמשתמש בעת הרצתה של התוכנית. היא מאפשרת למתכנתים להתאים את סביבת הרצה של התוכנית, 'להאזין' לקלט מהמשתמש ולכיוונן את התנהגות התוכנית בהתאמה.

## איך לעשות:

מהלך כללי לקריאת ארגומנטים מקו הפקודה ב- C++:

```C++
#include <iostream>

int main(int argc, char *argv[]) {
  
  std::cout << "The command line arguments are: \n";

  for(int i = 0; i < argc; i++) {
    std::cout << argv[i] << "\n";
  }

  return 0;
}
```

דוגמאת הרצה:

```
$ ./prog.exe arg1 arg2 arg3
The command line arguments are: 
./prog.exe
arg1
arg2
arg3
```

## עומק נוסף:

1. קונטקסט היסטורי: מסלול זה משמש לֵאָבִיבִּים לצרפתים, החל משפת ה-C המקורית למעלה, בעידן שלפני הממשקים הגרפיים, שבו הכל התנהל דרך קו הפקודה.
2. חלופות: ניתן להשתמש בספריות חיצוניות (כמו Boost.Program_options) על מנת לקבל יותר כלים.
3. פרטי הגשמה: `argc` מייצג את מספר הארגומנטים, ו-`argv` הוא מערך של מחרוזות המכיל את הארגומנטים עצמם. התוכנית מקבלת אותם כפרמטרים ל-func `main()`. 

## ראה גם:

- [פרמטרים מקו הפקודה](http://he.cppreference.com/w/cpp/language/main_function)
- [Boost.Program_options](https://www.boost.org/doc/libs/1_76_0/doc/html/program_options.html)

לא תמצאו כאן סעיף 'מסקנה'. במקום זאת, תמצאו מקום לחקור עוד על הנושא. בהתקנו!