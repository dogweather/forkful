---
title:                "C: קריאת משתני שורת פקודה."
simple_title:         "קריאת משתני שורת פקודה."
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה 

כמו כל דבר אחר בתכנות, כידוע שהבנת הדרך לקרוא ולעבד את משתני הפרמטרים מסוף הפקודה היא חיונית לכתיבת קוד בדרך יעילה. למרבה המזל, קריאת פרמטרים מהסדרית הפקודות (command line arguments) באמצעות שָׂפָה C היא חדשה ופשוטה לרוב. 

## איך לקרוא פרמטרים מהסדרית הפקודות 

כדי לקרוא פרמטרים מהסדרית הפקודות בשפת C, תצטרכו לשים לב לשני דברים חשובים: המערכת המפעילה שאתם משתמשים בה והטיפוסים של הפרמטרים. 

## חפירה עמוקה 

מערכת הפעלה שאתם משתמשים בה יכולה להשפיע על איך הפרמטרים יועבדו. למשל, במספר מערכות, תוכלו להתחשב במספר הגדול של פרמטרים שניתן להעביר צד אחד לצד אחר. בנוסף, אתם תראו דוגמאות של פלט (output) שאפשר לצפות כאשר פרמטרים רבים מודפסים. 

```C 
 #include<stdio.h> 

int main(int argc, char *argv[]) 
{ 
    int count; 
    printf("Number of command line arguments: %d\n", argc); 
    printf("-----------------------------------------------\n"); 
  
    // Loop through each command line argument 
    // ignoring the first argument (argv[0]) 
    for(count = 1; count < argc; count++) 
    { 
        printf("%d) %s\n", count, argv[count]); 
    } 
  
    return 0; 
} 
``` 

פלט משמעותי מהקוד לעיל יכול להיות כזה: 

```
λ ./filename one two three four five 

Number of command line arguments: 6 

----------------------------------------------- 

1) one 
2) two 
3) three 
4) four 
5) five 
```

כפי שניתן לראות, ניתן ללולא דרך כל פרמטר בכדי לבדוק אותו או לעבוד איתו כרצונכם. הוכחת שגיאות (error checking) גם יכולה להיות מיועדת לכל פרמטר כמעט, אם כי עם השגיה אחת חשוב לסיים את התוכנית. 

## ראה ג