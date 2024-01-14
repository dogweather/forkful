---
title:                "Java: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

מאחר ומעטים מנושאים בעולם התוכנה תולים כל כך הרבה על התקשורת בין שרתים, לוגיקה אבסטרקטית יכולה לקרוס בקלות. ישנו מתאם שמכנה שמו "HTTP" המאפשר לנו לתקשר עם שרתים פרוטוקוליים, והוא עובד ע"י בקשות ותגובות. כאשר תרצו לבצע בקשה משרת, ייתכן שתרצו גם להכניס תעודת זיהוי כדי להיות מורשים לגשת אליו. בבלוג הזה נלמד כיצד לשלוח בקשה HTTP עם אימות בסיסי כדי לשרת את המטרה הזאת.

## איך לעשות

כעת, ננסה להפוך את כל מילות הכבוד למספרים. נתחיל עם יצירת סטרימר HTTP עבור האתר שאנחנו רוצים לתקשר איתו.

```Java
// יצירת סטרימר HTTP
URL url = new URL("https://example.com"); // כתובת האתר בו אנחנו רוצים לתקשר
HttpURLConnection con = (HttpURLConnection) url.openConnection(); // פתיחת חיבור
con.setRequestMethod("GET"); // נערוך בקשת GET

// הוספת אימות בסיסי לבקשה
String username = "myusername"; // שם משתמש לאימות
String password = "mypassword"; // סיסמה לאימות
String credentials = username + ":" + password; // יצירת מחרוזת אימות
String base64Credentials = Base64.getEncoder().encodeToString(credentials.getBytes()); // המרת המחרוזת לקוד base64
con.setRequestProperty("Authorization", "Basic " + base64Credentials); // הוספת כעת הקוד הבסיסי לכותרת האימות של הבקשה

// קבלת תגובת החיבור
int status = con.getResponseCode(); // מצב התגובה
String message = con.getResponseMessage(); // הודעת התגובה
System.out.println(status + " " + message); // הצגת התגובה

// קריאת התוכן של התגובה
BufferedReader in = new BufferedReader(
    new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuffer content = new StringBuffer();
while ((inputLine = in.readLine()) != null) {
    content.append(inputLine);
}
in.close();
System.out.println(content.toString()); // הדפסת התוכן

// סגירת החיבור
con.disconnect();
```
כפי שאתם רואים בקוד הדוגמה, אנו יוצ