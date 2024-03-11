---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:11.988956-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1-Java \u05D4\u05D9\u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D5\u05D5\
  \u05D3\u05D0 \u05E9\u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05DE\u05EA\u05E0\
  \u05D4\u05D2 \u05DB\u05E6\u05E4\u05D5\u05D9 \u05EA\u05D7\u05EA \u05EA\u05E0\u05D0\
  \u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DB\u05D5\u05EA\u05D1\u05D9\u05DD \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05DE\u05E0\u05D5\u05E2 \u05D1\u05D0\u05D2\u05D9\u05DD\
  , \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D5\u05E0\u05D0\u05DC\u05D9\u05D5\u05EA \u05E0\u05E9\u05D0\u05E8\u05EA \u05E0\
  \u05DB\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D7\u05E8\u2026"
lastmod: '2024-03-11T00:14:12.578982-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  -Java \u05D4\u05D9\u05D0 \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D5\u05D5\u05D3\
  \u05D0 \u05E9\u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05DE\u05EA\u05E0\u05D4\
  \u05D2 \u05DB\u05E6\u05E4\u05D5\u05D9 \u05EA\u05D7\u05EA \u05EA\u05E0\u05D0\u05D9\
  \u05DD \u05E9\u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DB\u05D5\u05EA\u05D1\u05D9\u05DD \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05DE\u05E0\u05D5\u05E2 \u05D1\u05D0\u05D2\u05D9\u05DD, \u05DC\
  \u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\
  \u05D0\u05DC\u05D9\u05D5\u05EA \u05E0\u05E9\u05D0\u05E8\u05EA \u05E0\u05DB\u05D5\
  \u05E0\u05D4 \u05DC\u05D0\u05D7\u05E8\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות ב-Java היא על מנת לוודא שהקוד שלך מתנהג כצפוי תחת תנאים שונים. מתכנתים כותבים בדיקות כדי למנוע באגים, לוודא שהפונקציונאליות נשארת נכונה לאחר שינויים, ולקדם עקרונות של עיצוב תוכנה טוב.

## איך לעשות:
מפתחי Java משתמשים בעיקר בשני מסגרות לבדיקות: JUnit ו-TestNG. כאן, נתמקד ב-JUnit, הבחירה הפופולרית יותר לכתיבת בדיקות בשל פשטותה והתפוצה הרחבה שלה.

### הבסיס של JUnit

כדי להשתמש ב-JUnit בפרויקט Maven שלך, הוסף את התלות הבאה ל-`pom.xml` שלך:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

בדיקה בסיסית ב-JUnit נראית כך:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 אמור להשתוות ל-5");
    }
}
```

הרצת בדיקה זו תוכל לעבור, מה שמצביע על כך שהמתודה `add` פועלת כצפוי, או להיכשל, ולהציג הודעת שגיאה.

### מדמה עם Mockito

בתרחישים מהעולם האמיתי, אובייקטים לעיתים תלויים באובייקטים אחרים. Mockito הוא מסגרת מדמה פופולרית שעוזרת ביצירת אובייקטים מדומים למטרות בדיקה.

הוסף את Mockito לפרויקט Maven שלך:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

שימוש בסיסי עם Mockito:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // יצירת מדמה ל-UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // הגדרת התנהגות עבור אובייקט המדמה
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "מזהה המשתמש 1 אמור להיות john_doe");
    }
}
```

המדמה הזה מאפשר לנו לבדוק את `UserService` בלי הצורך ב-`UserRepository` אמיתי, וממקד את הבדיקה בלוגיקה שבתוך `UserService` עצמו.
