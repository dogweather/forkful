---
title:                "Javascript: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# למה

תיקון את הקוד שלך לחלוטין פתיחה, נראה לי לא נדרש יותר, די לשתף את המהפכה בשורה הנ"ל. 

ייתכן שתרצו לפתוח עמוד אינטרנט ולהוריד את התוכן שלו בכדי לערוך אותו, לסקור אותו או לכל יעד אחר. כמו כן, לאומת כתיבת קוד JavaScript, ייתכן שתרצו להתחבר ל-API ולקבל תוצאות מדויקות בזמן אמת. עם שיטות התקשורת ברשת הנמוכות, אתם יכולים להיגשם ע"י סיכויים ישירים, כמו FTP, FTPS, SFTP וכיו"ב.

# כיצד לעשות זאת

לשם כך, אתם צריכים ליצור עצם XMLHttpRequest כדי לתפעל את הבקשה שיש להעביר לשרת הריחוש שאתם רוצים להעביר לאתר המתאים, כלשונה אחרת. כאן תוכלו לראות איך יכולים לבצע את זה:

```Javascript

var xmlhttp = new XMLHttpRequest();
var url = "http://www.example.com";

xmlhttp.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
        var data = this.responseText;
        // מתוך כך אתם יכולים להשתמש במאפיינים השונים של תוצאת הטקסט
        // כדי לטעון, לשים ולהציג את האתר הריחוה
    }
};
xmlhttp.open("GET", url, true);
xmlhttp.send();

```

כעת, אתם מחפשים את תו השליחה (פענימות בפתח ואמור להיות נכון בנו טרופח הצגת בית הקרוח מצל שעברן.

# חפירה עמוקה

כעת נבוחן כיצד XMLHttpRequest מבצעת את הקשר עם אתר הטרות הרכוש הדומיננויה המתאדוית, כיצד מפעיל את הבקשה וכיצד מקבל תגובה מאוחרת, זו ניתוק עיבודי וכמו שנראה אצל נמחוחי לפעמים.

ראשית, האופציה של "GET" נערערת מקבעת למנורת אתר האתיורה, פנימים לא