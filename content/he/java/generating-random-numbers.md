---
title:                "Java: יצירת מספרים אקראיים"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# למה

כתיבת קוד פרומדי בג'אבה כוללת הרבה אלגוריתמים שימושיים לטיפול במגוון של תערובות רנדומליות. יישומם של קוד זה יכול להיות מאוד שימושי לאנשים שמחפשים דרך ליצור מחרוזות רנדומליות או מספרים רנדומליים עבור משחקים, טסטים, או כל מטרה אחרת.

# איך לעשות זאת?

הנה כמה דוגמאות להפקת מספרים רנדומליים בג'אבה:

```Java
// ייבוא מחלקת הולדר לצורך הפקת מספרים רנדומליים
import java.util.Random;

// פעולה זו יוצרת מספר רנדומלי בין 0 ל-10 ומדפיסה את התוצאה
public static void generateRandomNumber() {
	Random rand = new Random();
	int randomNum = rand.nextInt(10);
	System.out.println(randomNum);
}

// יצירת מערך עם 10 ערכים רנדומליים בין 0 ל-100
public static void generateRandomArray() {
	Random rand = new Random();
	int[] randomArray = new int[10];
	for(int i=0; i<randomArray.length; i++) {
		randomArray[i] = rand.nextInt(100);
		System.out.println(randomArray[i]);
	}
}

// יצירת מחרוזת רנדומלית באורך 6 תווים
public static void generateRandomString() {
	Random rand = new Random();
	String randomString = "";
	for(int i=0; i<6; i++) {
		// נוסיף תווים אקראיים לאורך המחרוזת באמצעות קידומת של אותיות גדולות או קטנות בין 65 ל-122
		randomString += (char)(rand.nextInt((122-65) + 1) + 65);
	}
	System.out.println(randomString);
}
```

הנה כמה תוצאות ייצור רנדומלי של הקוד שלנו:

```
3
[36, 12, 78, 99, 47, 22, 58, 60, 53, 79]
jKbkuB
```

אתה יכול גם לשנות ולהתאים את הקוד לפי הצורך שלך כדי ליצור נתונים רנדומליים בכל סוג של פורמט.

# דיב דייב (עמוק)

התפתחות מקוד רנדומלי נעשתה נדירה במהלך ההיסטוריה של תכנות המחשב כי היצירה של פונק