---
title:                "Java: שליחת בקשת HTTP"
simple_title:         "שליחת בקשת HTTP"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מדוע
מעבר לתוכניות פשוטות של דוא"ל וגלישת אינטרנט, הפעילות שלנו באינטרנט כלל נעשתה תוך שליחת בקשות HTTP. דוגמאות לכך כוללות כתבות בבלוגים, אתרי חנויות אינטרנטיות, ואפליקציות תוכנה שנגישות דרך האינטרנט. לכן, לדעת כיצד לשלוח בקשת HTTP נחשב לכישור חשוב לכל מתכנת ג'אווה. 

## כיצד לעשות זאת

לשולחן עבודה שלנו מותקנת כרגע של התוכנה החשובה ביותר של ג'אווה - JDK. מפה אנו יכולים ליצור מימוש של מחלקת HttpURLConnection, המייצגת חיבור רשת של בקשה HTTP. אם אני רוצה לקבל את תוכן דף אינטרנט מזין רגיל, אני יכול לכתוב את הקוד הבא:

```Java
import java.net.*;
import java.io.*;

public class HttpExample {
 public static void main(String[] args) throws Exception {
  URL url = new URL("https://blog.example.com"); // כתובת האתר שאנו רוצים לשלוח לו בקשת HTTP
  HttpURLConnection connection = (HttpURLConnection) url.openConnection();
  connection.setRequestMethod("GET"); // כאן אנו מגדירים מתיווך של הבקשה HTTP השליחה - במקרה זה, נשלח בקשה סטנדרטית
  int responseCode = connection.getResponseCode(); // מקבל קוד תגובת HTTP עבור בקשה שנשלחה
  System.out.println("קוד תגובה: " + responseCode); // מדפיס קוד תגובה
  BufferedReader input = new BufferedReader(
    new InputStreamReader(connection.getInputStream()));
  String inputLine;
  while((inputLine = input.readLine()) != null) {
    System.out.println(inputLine); // מדפיס את תוכן הדף שקיבלנו כתגובה לבקשה שנשלחה
  }
  in.close();
 }
}
```

מפה, אנחנו יכולים לראות שאנחנו מוודאים את קוד התגובה של הבקשה, ואז מדפיסים את התוכן של הדף שקיבלנו כתגובה. ניתן להתאים את הקוד הזה כדי לשלוח בקשות שונות יותר, כמו POST או PUT. 

## הכנסה עמוקה

ע