---
title:                "עובדים עם JSON"
html_title:           "C++: עובדים עם JSON"
simple_title:         "עובדים עם JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-json.md"
---

{{< edit_this_page >}}

מדוע: עדכנו על גם- יעיל בגישה עם נתונים בפורמט קומפקטי ונקי. אם אתה עובד עם אפליקציות מתוכנתות או פרויקטים שמנוהלים ע"י מכבי ההגנת נתונים כמו Ajax או Fetch API, סירת הנתונים שלך לחוצן ה-Microsoft JSON יהיה פרקטי לחלוטין וכמצטבר לתרומתך כשפת תכנות המודרנית של לכתתור לעובדים.

כיצד לעבוד עם JSON:
```C++
#include <iostream>
#include <json/json.h>

int main() {
   //יצירת אובייקט JSON ריק
   Json::Value json_obj;
   //הגדרת ערך בשם המפתח "name"
   json_obj["name"] = "John";
   //הגדרת ערך בשם המפתח "age"
   json_obj["age"] = 30;
   //הדפסת האובייקט בפורמט כתב
   std::cout << json_obj.toStyledString() << std::endl;

   //שליפת נתונים מקובץ קיים של JSON
   Json::Value json_data;
   //פתיחת הקובץ במצב קריאה
   std::ifstream json_file("data.json", std::ifstream::binary);
   json_file >> json_data;
   //הדפסת הערך של המפתח "name"
   std::cout << json_data["name"] << std::endl;
   //הדפסת הערך של המפתח "age"
   std::cout << json_data["age"] << std::endl;
}
```

ריבית עולה:

עבור עורכי גריפ או कי ynton ניתן לעדכן או לטטיח רשת נתונים כלשהו עם נתונים הטכנולוגיים בפורמט JSON. רק לבחור את המתאם שנכנס למכביות ולפתוח EAuth כך שעם פעולה דיגיטלית או תיבת רכישה .הדמיה של הנתונים המשתתפים תהיה אפשרית לכתב כדי לשרוד כמות בגורם גבוליה החשוב

תהליכי עזרי אריחים:

כאשר אנחנו עובדים עם נתונים בפורמט JSON, חשוב לציין שהנתונים מתכתבים לתיאורי פונקציות השולט במערכות. V דרך בלוי כ