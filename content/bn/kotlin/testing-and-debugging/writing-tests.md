---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:41:22.965166-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

Kotlin-এ টেস্ট লিখা মানে এমন কোড স্নিপেট তৈরি করা যা আপনার সফটওয়্যার মডিউলগুলোর কার্যকরী সঠিকতা স্বয়ংক্রিয়ভাবে যাচাই করে, এবং নিশ্চিত করে যে তারা প্রত্যাশিত ভাবে কাজ করছে। প্রোগ্রামাররা বাগগুলি দ্রুত ধরার জন্য, কোড রিফ্যাক্টরিং সহজ করার জন্য, এবং সফটওয়্যার কম্পোনেন্টগুলো কীভাবে কাজ করার কথা তার উপর নথি প্রদানের জন্য এটি করে থাকেন।

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
