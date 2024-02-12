---
title:                "כתיבת בדיקות"
aliases:
- /he/java/writing-tests.md
date:                  2024-02-03T19:31:11.988956-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
