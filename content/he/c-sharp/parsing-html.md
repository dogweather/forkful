---
title:                "C#: פירוק קוד HTML"
simple_title:         "פירוק קוד HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

בכל תחום של פיתוח תוכנה, כישור לקרוא ולהתאים את הנתונים הוא חיוני. לעתים קרובות יש צורך לפענח מידע שמצוי בתוך דפי HTML כדי לאפשר למשתמשים להתמודד עם נתונים רלוונטיים. פירושו של פירסום המידע הוא להיות יכול לקבל את המידע הנוף ולהפריד אותו למידע אחר כדי לאפשר לקרוא את המידע בצורה יותר נוחה ויעילה למשתמשים

##  למה

 העובדה היא שההיתר לנסות ולפרק דפי HTML יכול להיות נורא מליץ בפעם הראשונה, בין אם אתה מנסה להתמודד עם כמה מסמכי HTML מקור, או אם אתה צריך לפרק 100 מסמכי מידע ללא מאמץ. אבל אחרי שאתה לומד את זה ומצליח לעשות את זה, זה חלק מתוך ארוחת הבוקר 

##  איך לעשות זאת

אין צורך לחשוש מכך שפריסת דף HTML יהיו קשים לפענח. בעצם, עם כמה שורות קוד פשוטות, באמצעות HtmlAgilityPack כניסה אתה יכול להיות בטוח שתכונו את המידע שאתה רוצה

דוגמה לתכונ את הנתונים בתוך קוד C# נתונים HTML:

```
  
using HtmlAgilityPack;
using System.Net;
using System.IO;
	
public static void Main() { 
	
	string url = "https://www.example.com";
	
	// החל מקבל את הדף HTML"
	
	WebRequest httpRequest = (HttpWebRequest)WebRequest.Create(url);
	var httpResponse = (HttpWebResponse)httpRequest.GetResponse();
	
	if (httpResponse.StatusCode == HttpStatusCode.OK) {
		
		var htmlDocument = new HtmlDocument();
		
		//בטעינת HTML כמדומנט:
		
		htmlDocument.Load(httpResponse.GetResponseStream(), true);
		
		//בדיקה
		htmlDocument.Save("/Users/username/Desktop/example.html");
		
		// להפעיל את שורות שליטה לפריסה UP את HTML R
		var table = htmlDocument.DocumentNode.SelectSingleNode("//div[@class='table-holder']");
		
		// הוספת טון אחר כל דף R
		var rows = table?.SelectNodes("//div[@class='row']");
		
		// פלט S:
		foreach (var row in rows) {
			var rowData = row.InnerText;
			Console.WriteLine(rowData);
		}

	}
}
```

פלט נ