---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:41:19.393030-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
