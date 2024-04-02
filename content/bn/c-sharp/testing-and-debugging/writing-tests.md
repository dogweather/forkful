---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:19.393030-06:00
description: "C# \u098F \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B8\u09CD\u09AC\u09AF\u09BC\u0982\u0995\u09CD\u09B0\u09BF\u09AF\
  \u09BC \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\
  \u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4\
  \ \u0995\u09B0\u09BE \u09AF\u09C7 \u098F\u099F\u09BF \u09AA\u09CD\u09B0\u09A4\u09CD\
  \u09AF\u09BE\u09B6\u09BF\u09A4 \u0986\u099A\u09B0\u09A3 \u0995\u09B0\u099B\u09C7\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.040882-06:00'
model: gpt-4-0125-preview
summary: "C# \u098F \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B8\u09CD\u09AC\u09AF\u09BC\u0982\u0995\u09CD\u09B0\u09BF\u09AF\
  \u09BC \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\
  \u09B0 \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2\u09BF\u099F\u09BF \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4\
  \ \u0995\u09B0\u09BE \u09AF\u09C7 \u098F\u099F\u09BF \u09AA\u09CD\u09B0\u09A4\u09CD\
  \u09AF\u09BE\u09B6\u09BF\u09A4 \u0986\u099A\u09B0\u09A3 \u0995\u09B0\u099B\u09C7\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BE \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u2026"
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কি এবং কেন?

C# এ টেস্ট লিখা মানে স্বয়ংক্রিয় স্ক্রিপ্ট তৈরি করে আপনার কোডের ফাংশনালিটি যাচাই করা, নিশ্চিত করা যে এটি প্রত্যাশিত আচরণ করছে। প্রোগ্রামাররা এটা করে থাকে বাগগুলি আগে ভাগে ধরার জন্য, কোড রিফ্যাক্টরিংকে সহজীকরণের জন্য, এবং নতুন পরিবর্তনগুলি যাতে বিদ্যমান ফাংশনগুলিকে না ভেঙে ফেলে তা নিশ্চিত করার জন্য, এর ফলে সফ্টওয়্যারের গুণগত মান এবং নির্ভরযোগ্যতা বৃদ্ধি পায়।

## কিভাবে:

C# ডেভেলপাররা মূলত NUnit অথবা xUnit ফ্রেমওয়ার্ক ব্যবহার করে টেস্ট লিখে থাকেন কারণ এদের নমনীয়তা এবং বিস্তৃত ফিচার সেটের জন্য। এখানে একটি বেসিক উদাহরণ দেওয়া হলো NUnit ব্যবহার করে একটি সাধারণ যোগ ফাংশন টেস্ট করার:

১. **NUnit এবং NUnit3TestAdapter ইনস্টল করুন** NuGet প্যাকেজ ম্যানেজার অথবা .NET CLI ব্যবহার করে:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

২. **একটি C# ক্লাস লাইব্রেরি** প্রজেক্ট তৈরি করুন যদি আপনি এরমধ্যে না করে থাকেন।

৩. **একটি সাধারণ ফাংশন লিখুন** টেস্ট করার জন্য। উদাহরণস্বরূপ, `Calculator` নামের একটি ক্লাসের মধ্যে একটি যোগ মেথড:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

৪. **NUnit ব্যবহার করে একটি টেস্ট ক্লাস লিখুন**:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

৫. **আপনার IDE এর টেস্ট রানার অথবা .NET CLI ব্যবহার করে টেস্টটি চালান**:
```powershell
dotnet test
```

### নমুনা আউটপুট:

ধরে নেওয়া যাক, আপনার টেস্টটি পাস হয়, আপনি এরূপ একটি আউটপুট দেখতে পাবেন:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### xUnit ব্যবহার:

আপনি যদি xUnit পছন্দ করেন, সেটআপটি অনুরূপ NUnit এর মত। এখানে আপনি xUnit ব্যবহার করে `Calculator` ক্লাসের জন্য টেস্ট উদাহরণটি কিভাবে পুনর্লিখন করবেন:

১. **xUnit এবং xUnit.runner.visualstudio ইনস্টল করুন**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

২. **xUnit ব্যবহার করে একটি টেস্ট ক্লাস লিখুন**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

৩. **.NET CLI ব্যবহার করে অথবা আপনার IDE এর ইন্টিগ্রেটেড টেস্ট রানারে টেস্টটি চালান**।

NUnit এবং xUnit উভয়ই প্যারামিট্রাইজড টেস্টিং, সেটআপ/টিয়ারডাউন অপারেশন এবং টেস্টগুলিকে বিভাগে বিন্যাস করার মতো ক্ষমতাশালী বৈশিষ্ট্য প্রদান করে, যা C# প্রোগ্রামারের টুলকিটে কোডের গুণমান এবং ফাংশনালিটি নিশ্চিত করার জন্য অপরিহার্য টুলস হিসেবে গণ্য করা হয়।
