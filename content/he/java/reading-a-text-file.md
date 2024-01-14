---
title:                "Java: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

##למה
 למה לקרוא קובץ טקסט?

הקריאה של קבצי טקסט נחשבת לנפוצה בתכנות ג'אווה, והיא משמשת למגוון רחב של מטרות כמו טעינת נתונים והצגתם, העברת מידע בין אפליקציות ועוד. אז למה לקרוא קובץ טקסט? כי זה הדרך הטובה ביותר לגשת ולעבד את המידע הנמצא בקובץ בצורה תואמת לסגנון התכנות העימותי של ג'אווה.

## איך לעשות זאת
הנה כמה דגמי קוד לקריאת קובץ טקסט בג'אווה.

```java
BufferedReader reader = new BufferedReader(new FileReader("textFile.txt"));
String line = reader.readLine();

while (line != null) {
	System.out.println(line);
	line = reader.readLine();
}

reader.close();
```
####פלט:
```
שורה אחת מהקובץ היהודי
שורה שנייה מהקובץ היהודי
שורה שלישית מהקובץ היהודי
```

```java
try {
	List<String> lines = Files.readAllLines(Paths.get("textFile.txt"));
	for (String line : lines) {
		System.out.println(line);
	}
} catch (IOException e) {
	e.printStackTrace();
}
```
####פלט:
```
שורה אחת מהקובץ היהודי
שורה שנייה מהקובץ היהודי
שורה שלישית מהקובץ היהודי
```

##סערה מעמיקה
כעת שאנחנו ראינו כמה דרכים לקרוא ולהדפיס את תוכן הקובץ הטקסט בג'אווה, בואו נעמיק קצת יותר בכיצד התכנית שלנו באמת עובדת.

כאשר בונים את מעטפת הקובץ, נפתח קשר עם המערכת האופרטיבית באמצעות הפקודת "new FileReader("textFile.txt")". כאן אנחנו מגדירים את שם הקובץ שאנחנו רוצים לקרוא ומשנים את זה לפקודת הקריאה לשורה הבאה.

לאחר מכן, אנו משתמשים באובייקט של BufferedReader כדי לקרוא את התוכן בקובץ. כאן יש חשיבות רבה לשימוש