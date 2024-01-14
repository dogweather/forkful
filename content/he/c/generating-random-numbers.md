---
title:                "C: הפקת מספרים אקראיים"
simple_title:         "הפקת מספרים אקראיים"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# למה

בתכנות, יצירת מספרים אקראיים היא פעולה חשובה ונפוצה. מספרים אקראיים יכולים להיות מועילים במגוון רחב של אפליקציות, כגון מבחנים, משחקים ומודלים סטטיסטיים. בכתבה זו, אנו נכיר איך ליצור מספרים אקראיים בשפת סי ומהם האפשרויות השונות לעשות זאת.

# איך לעשות

כדי ליצור מספרים אקראיים בשפת סי, אנו משתמשים בפונקציה המובנית rand(). לפני שנבין איך פועלת פונקצית rand(), נחקור דוגמאות של יישומים שונים של מספרים אקראיים בשפת סי.

### דוגמה 1:
```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
  int i;
  // נגריל 5 מספרים אקראיים ונדפיס אותם 
  for(i = 0; i < 5; i++)
  {
    printf("%d\n", rand());
  }
  return 0;
}
```

פלט:
```
1804289383
846930886
1681692777
1714636915
1957747793
```

### דוגמה 2:
```C
#include <stdio.h>
#include
stdlib.h>

int main()
{
  int i;
  // נגרום לפונקצית rand() להחזיר מספרים אקראיים רק בין 1 ל-10 ונדפיס אותם 
  for(i = 0; i < 5; i++)
  {
    printf("%d\n", (rand() % 10) + 1);
  }
  return 0;
}
```

פלט:
```
7
2
10
4
6
```

### דוגמה 3:
```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
  int i;
  // נגריל 6 מספרים אקראיים ונשווה את המספר האחרון למספר הקודם כדי לקבוע אם המספרים זהים או לא
  int num, prev_num = 0;
  for(i = 0; i < 6; i++)
  {
    num = rand();
    if(num == prev_num)
    {
      printf("המספרים הם זהים!\n");
    }
    else
    {
      printf("המספרים הם שונים!\n");
    }
    prev_num = num;
  }
  return 0;
}
```

פלט:
```
המספרים הם שונים!
המספרים הם שונים!
המס