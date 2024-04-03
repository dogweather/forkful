---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:34.341521-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE \u09A1\
  \u09C7\u09AD\u09C7\u09B2\u09AA\u09BE\u09B0\u09B0\u09BE \u09AE\u09C2\u09B2\u09A4\
  \ \u09A6\u09C1\u099F\u09BF \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\
  \u09B0\u09C7\u09AE\u0993\u09DF\u09BE\u09B0\u09CD\u0995 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7: JUnit \u098F\u09AC\u0982 TestNG\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7, \u0986\u09AE\u09B0\u09BE JUnit \u09A8\u09BF\u09DF\u09C7 \u0986\
  \u09B2\u09CB\u099A\u09A8\u09BE \u0995\u09B0\u09AC, \u09AF\u09BE \u098F\u09B0 \u09B8\
  \u09BE\u09A6\u09BE\u09B8\u09BF\u09A7\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09BE\u09AA\u0995\u2026"
lastmod: '2024-03-17T18:47:43.908002-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE \u09A1\u09C7\u09AD\u09C7\u09B2\u09AA\u09BE\u09B0\
  \u09B0\u09BE \u09AE\u09C2\u09B2\u09A4 \u09A6\u09C1\u099F\u09BF \u099F\u09C7\u09B8\
  \u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09DF\u09BE\u09B0\u09CD\
  \u0995 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
জাভা ডেভেলপাররা মূলত দুটি টেস্টিং ফ্রেমওয়ার্ক ব্যবহার করে: JUnit এবং TestNG। এখানে, আমরা JUnit নিয়ে আলোচনা করব, যা এর সাদাসিধে ব্যবহার এবং ব্যাপক গ্রহণযোগ্যতার কারণে টেস্ট লিখতে জনপ্রিয় বিকল্প।

### JUnit মূলকথা
আপনার Maven প্রজেক্টে JUnit ব্যবহার করতে, নিম্নলিখিত ডিপেন্ডেন্সি আপনার `pom.xml` এ যোগ করুন:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

JUnit এ একটি বেসিক টেস্ট এরকম দেখায়:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 এর মান 5 হওয়া উচিত");
    }
}
```

এই টেস্টটি সম্পাদন করলে, ফলাফল হয় পাস, যেটা দেখায় `add` মেথডটি প্রত্যাশিত অনুসারে কাজ করে, অথবা ফেইল হয়, যা একটি ত্রুটি বার্তা দেখায়।

### Mockito দিয়ে Mocking
বাস্তব দুনিয়ায়, সাধারণত অবজেক্টগুলো অন্যান্য অবজেক্টের উপর নির্ভরশীল থাকে। Mockito একটি জনপ্রিয় mocking ফ্রেমওয়ার্ক যা টেস্টিং এর জন্য mock অবজেক্ট তৈরি করতে সাহায্য করে।

আপনার Maven প্রজেক্টে Mockito যোগ করুন:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

Mockito সহ একটি সাধারণ ব্যবহারের উদাহরণ:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Create a mock UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // Mock অবজেক্টের আচরণ নির্ধারণ
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "User ID 1 এর জন্য john_doe থাকা উচিত");
    }
}
```

এই mock এর মাধ্যামে আমরা `UserService` টি টেস্ট করতে পারি একটি আসল `UserRepository` ছাড়াই, যেটা `UserService` এর মধ্যে থাকা যুক্তি পরীক্ষা করতে সাহায্য করে।
