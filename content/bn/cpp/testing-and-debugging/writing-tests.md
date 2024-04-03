---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:58.329357-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: #."
lastmod: '2024-03-17T18:47:44.368870-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:


### গুগল টেস্ট ফ্রেমওয়ার্ক ব্যবহার করা
C++ এ টেস্ট লেখার জন্য জনপ্রিয় তৃতীয়-পক্ষের লাইব্রেরিগুলির মধ্যে গুগল টেস্ট অন্যতম। প্রথমে, আপনাকে গুগল টেস্ট ইনস্টল করতে হবে এবং এটি আপনার প্রজেক্টের সাথে লিঙ্ক করতে হবে। সেট আপ করার পর, আপনি টেস্ট কেস লেখা শুরু করতে পারেন।

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

কোডটি একটি ফাইলে সংরক্ষণ করুন, এবং এটি জিপ্লাসপ্লাস কম্পাইলারের সাথে কম্পাইল করুন, গুগল টেস্ট লাইব্রেরিটি লিঙ্ক করে। সব কিছু ঠিকঠাক মতো সেট আপ করা হলে, ফলাফলের নির্বাহযোগ্য চালানো হবে টেস্টটি চালাবে, এবং যদি `add` ফাংশনটি প্রত্যাশামতো কাজ করে, আপনি কিছু দেখতে পাবেন যেমন:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### ক্যাচ2 ব্যবহার করা
C++ এর জন্য আরেকটি জনপ্রিয় টেস্টিং ফ্রেমওয়ার্ক হল ক্যাচ2। এর একটি সহজতর সিনট্যাক্স রয়েছে এবং সাধারণত একটি লাইব্রেরির বিপরীতে লিঙ্ক করার প্রয়োজন হয় না (শুধুমাত্র হেডার)। এখানে ক্যাচ2 দিয়ে একটি সহজ টেস্ট লেখার একটি উদাহরণ দেওয়া হলো:

```cpp
#define CATCH_CONFIG_MAIN  // This tells Catch to provide a main() - only do this in one cpp file
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Integers are multiplied", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

এই টেস্টটি কম্পাইল ও চালানোর পরে, ক্যাচ2 পরিষ্কার আউটপুট সরবরাহ করে যা ইঙ্গিত দেয় টেস্টটি উত্তীর্ণ হয়েছে বা ব্যর্থ হয়েছে, এবং ব্যর্থতার ডিবাগের জন্য প্রয়োজনীয় যেকোনো তথ্য প্রদান করে:

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

এই উদাহরণগুলি দেখায় কিভাবে আপনার C++ ডেভেলপমেন্ট ওয়ার্কফ্লোতে টেস্টিং ফ্রেমওয়ার্কগুলি একীভূত করা আপনার কোডের বিশ্বাসযোগ্যতা এবং রক্ষণাবেক্ষণকে জোরদার করতে পারে।
