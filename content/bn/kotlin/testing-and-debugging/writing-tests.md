---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:22.965166-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Kotlin \u09AC\u09BF\u09AD\u09BF\
  \u09A8\u09CD\u09A8 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\
  \u0995\u09C7\u09B0 \u09B8\u09B9\u09BE\u09AF\u09BC\u09A4\u09BE\u09AF\u09BC \u099F\
  \u09C7\u09B8\u09CD\u099F-\u099A\u09BE\u09B2\u09BF\u09A4 \u0989\u09A8\u09CD\u09A8\
  \u09AF\u09BC\u09A8 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7, \u09AF\
  \u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\
  \u09C7 \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09DF \u09B9\u09B2 JUnit, Kotest, \u098F\
  \u09AC\u0982 \u09AE\u0995\u09BF\u0982-\u098F\u09B0 \u099C\u09A8\u09CD\u09AF MockK\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.997308-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AB\u09CD\u09B0\u09C7\
  \u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\u09C7\u09B0 \u09B8\u09B9\u09BE\u09AF\
  \u09BC\u09A4\u09BE\u09AF\u09BC \u099F\u09C7\u09B8\u09CD\u099F-\u099A\u09BE\u09B2\
  \u09BF\u09A4 \u0989\u09A8\u09CD\u09A8\u09AF\u09BC\u09A8 \u09B8\u09AE\u09B0\u09CD\
  \u09A5\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7\
  \ \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\
  \u09DF \u09B9\u09B2 JUnit, Kotest, \u098F\u09AC\u0982 \u09AE\u0995\u09BF\u0982-\u098F\
  \u09B0 \u099C\u09A8\u09CD\u09AF MockK\u0964 \u098F\u0996\u09BE\u09A8\u09C7 JUnit\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কীভাবে:
Kotlin বিভিন্ন ফ্রেমওয়ার্কের সহায়তায় টেস্ট-চালিত উন্নয়ন সমর্থন করে, যার মধ্যে সবচেয়ে জনপ্রিয় হল JUnit, Kotest, এবং মকিং-এর জন্য MockK। এখানে JUnit ব্যবহার করে একটি সাধারণ উদাহরণ দেওয়া হল:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `adds two numbers`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**স্যাম্পল আউটপুট**

```text
টেস্ট পাস হয়েছে।
```

Kotest ব্যবহার করে আরও উন্নত টেস্টিং পদ্ধতির জন্য, যা আরও বেশি Kotlin-ধর্মী টেস্ট লিখার স্টাইল দান করে, নীচের উদাহরণটি দেখুন:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "adding 2 and 3 should return 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

মক্স ব্যবহার করে MockK দ্বারা টেস্টিং:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `get data returns mocked data`() {
        every { repository.getData() } returns "Mocked Data"

        val result = service.getData()

        assertEquals("Mocked Data", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**স্যাম্পল আউটপুট**

```text
টেস্ট পাস হয়েছে।
```

এই উদাহরণগুলি Kotlin-এ ইউনিট টেস্ট লেখার মৌলিক ধারণা প্রদর্শন করে। আপনার অ্যাপ্লিকেশন বাড়ার সাথে সাথে, প্রতিটি ফ্রেমওয়ার্ক দ্বারা সরবরাহিত আরও উন্নত টেস্টিং পদ্ধতি এবং সরঞ্জাম অন্বেষণ করার বিষয়ে বিবেচনা করুন।
