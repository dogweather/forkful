---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:16.481981-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  -\u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u0996\u09C1\u0981\u099C\
  \u09C7 \u09AA\u09BE\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u09C7\u0987\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09AE\u09CB\u099F \u0995\u09A4\
  \u0997\u09C1\u09B2\u09BF \u0985\u0995\u09CD\u09B7\u09B0 \u0986\u099B\u09C7 \u09A4\
  \u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0987\u09A8\u09AA\u09C1\u099F\
  \ \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE, \u0985\u0995\u09CD\u09B7\u09B0\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B2\u09C1\u09AA \u099A\u09BE\u09B2\
  \u09BE\u09A8\u09CB, \u099F\u09C7\u0995\u09CD\u09B8\u099F\u2026"
lastmod: '2024-03-17T18:47:44.313353-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F\
  \u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u0996\u09C1\u0981\u099C\u09C7\
  \ \u09AA\u09BE\u0993\u09DF\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B8\u09C7\u0987 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09AE\u09CB\u099F \u0995\u09A4\u0997\
  \u09C1\u09B2\u09BF \u0985\u0995\u09CD\u09B7\u09B0 \u0986\u099B\u09C7 \u09A4\u09BE\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0987\u09A8\u09AA\u09C1\u099F \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE, \u0985\u0995\u09CD\u09B7\u09B0\u09C7\
  \u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B2\u09C1\u09AA \u099A\u09BE\u09B2\u09BE\
  \u09A8\u09CB, \u099F\u09C7\u0995\u09CD\u09B8\u099F\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিং-এর দৈর্ঘ্য খুঁজে পাওয়া মানে সেই স্ট্রিং-এ মোট কতগুলি অক্ষর আছে তা বের করা। প্রোগ্রামাররা ইনপুট যাচাই করা, অক্ষরের মধ্যে লুপ চালানো, টেক্সট সাজানো, অথবা ডাটা গতিশীলভাবে পরিচালনা করা ইত্যাদি কারণে এটি করে থাকে।

## কিভাবে:
```Arduino
void setup() {
  Serial.begin(9600); // সিরিয়াল যোগাযোগ শুরু করা
  String myString = "Hello, Arduino!"; // আপনার স্ট্রিং এখানে
  int stringLength = myString.length(); // স্ট্রিং-এর দৈর্ঘ্য নির্ণয়
  Serial.print("The length of the string is: ");
  Serial.println(stringLength); // দৈর্ঘ্য প্রদর্শন
}

void loop() {
  // এখানে কিছু করার নেই।
}
```
নমুনা আউটপুট:
```
স্ট্রিং-এর দৈর্ঘ্য হল: 15
```

## গভীর ডুব
পুরানো দিনে, C প্রোগ্রামাররা `<string.h>` থেকে `strlen()` ফাংশন ব্যবহার করে নাল-টার্মিনেটর পর্যন্ত অক্ষর গণনা করত। Arduino-র জগতে, `String` ক্লাস `length()` মেথডের সাথে জীবন সহজ করে দেয়। তবে মনে রাখবেন, সীমিত মেমরি সম্পদ সময়ের সাথে সাথে `String` অবজেক্ট ব্যবহার করলে ফ্রাগমেন্ট হয়ে যেতে পারে। বিকল্প? চার এরে (C-স্টাইল স্ট্রিং), যা মেমরির পক্ষে বেশি বান্ধব কিন্তু পরিচালনা করা ট্রিকি।

বৃহত্তর প্রকল্পগুলির জন্য, সবসময় মেমরি ম্যানেজমেন্ট বিবেচনা করুন। `length()` মেথডে কোনো অতিরিক্ত কম্পিউটিং প্রয়োজন হয় না—`String` অবজেক্ট নিজের আকারের হিসাব রাখে। অভ্যন্তরীণভাবে, `length()` একটি দ্রুত লুক-আপ, অক্ষর গণনা নয়। সেটি দক্ষ! কিন্তু, যদি আপনি মেমরি সম্পদে কম থাকেন, তবে চার এরে এবং ম্যানুয়াল দৈর্ঘ্য গণনা দিয়ে পুরানো দিনের `strlen()` এ ফিরে যান।

## আরো দেখুন
- Arduino `String` রেফারেন্স: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Arduino `strlen()` ফাংশন C-স্টাইল স্ট্রিংগুলির জন্য: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- Arduino-তে `String` এবং চার এরে নিয়ে আলোচনা: https://forum.arduino.cc/t/string-vs-char-array/678207
